package org.backend;

import com.ericsson.otp.erlang.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.springframework.stereotype.Component;

import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Map;

@Component
public class ErlangController implements Runnable {

    private final String cookie = "cookie";
    private final String javaNodeName = "java_node";
    private final String erlangNodeName = "master@localhost";

    private OtpConnection otpConnection;
    private OtpErlangObject supervisorPid;
    private OtpErlangPid javaPid;

    private Thread erlangControllerThread;
    private Process erlangProcess;


    @PostConstruct
    public void init() {
        erlangControllerThread = new Thread(this);
        erlangControllerThread.start();
    }

    @Override
    public void run() {

        // TODO mettere tutto asynch in erlang
        while (!Thread.currentThread().isInterrupted()) {
            try {

                if (otpConnection != null && otpConnection.isConnected() && otpConnection.msgCount() > 0) {
                    OtpErlangObject msg = otpConnection.receive();
                    MessageQueues.erlangQueue.put(msg.toString());
                    System.out.println("Result: " + msg);
                }

                String command = MessageQueues.webSocketQueue.poll();
                if (command != null) {
                    executeCommand(command);
                }

            } catch (Exception e) {   // TODO better handle exception
                System.out.println("[JAVA] Erlang message discard, reason: " + e.getMessage());
            }
        }
    }


    private void executeCommand(String CommandJSON) {

        ObjectMapper objectMapper = new ObjectMapper();
        Command command;

        try {
            command = objectMapper.readValue(CommandJSON, Command.class);
        } catch (JsonProcessingException e) {
            System.out.println("Command discarded with reason: " + e.getMessage());
            return;
        }

        System.out.println(command);

        if (command.isValid()) {
            try {
                execute(command);
            } catch (OtpAuthException e) {
                throw new RuntimeException(e);
            } catch (OtpErlangExit e) {
                throw new RuntimeException(e);
            } catch (IOException e) {
                throw new RuntimeException(e);
            } catch (InterruptedException | OtpErlangException e) {
                throw new RuntimeException(e);
            }
        } else {
            System.out.println("Command discarded, bad formatted");  // TODO send it to the session that submit the command
        }
    }

    private void execute(Command command) throws OtpAuthException, OtpErlangException, IOException, InterruptedException {

        switch (command.command) {
            case "start" -> startErlangMaster();   // TODO start only if not already started, also stopped
            case "stop" -> stopErlangMaster();
            case "train" -> train(command);
        }
    }

    private void train(Command trainCommand) throws IOException, OtpAuthException, OtpErlangException {

        System.out.println();
        otpConnection.sendRPC("master_api", "train", new OtpErlangList(
                new OtpErlangObject[]{
                        new OtpErlangInt(trainCommand.getTrainEpochs()),
                        new OtpErlangDouble(trainCommand.getAccuracy())
                }
        ));
        otpConnection.receiveRPC();  // async call

    }

    private void startErlangMaster() throws IOException, InterruptedException, OtpAuthException, OtpErlangExit {
        String beamPath = System.getProperty("user.dir") + "/../Erlang";
        erlangProcess = ErlangHelper.startErlangNode(beamPath, cookie, erlangNodeName, 10000);

        OtpSelf self = new OtpSelf(javaNodeName, cookie);
        OtpPeer other = new OtpPeer(erlangNodeName);
        this.otpConnection = self.connect(other);
        this.javaPid = self.pid();

        otpConnection.sendRPC("master_supervisor", "start_link_shell", new OtpErlangList(javaPid));
        supervisorPid = otpConnection.receiveRPC();
        System.out.println("Result: " + supervisorPid);
    }

    @PreDestroy
    public void cleanup() throws InterruptedException, OtpAuthException, OtpErlangExit, IOException {
        if (erlangControllerThread.isAlive()) {  // TODO check if important isAlive
            erlangControllerThread.interrupt();
            erlangControllerThread.join();
            stopErlangMaster();
        }
        System.out.println("ErlangController shutdown complete.");
    }


    private void stopErlangMaster() throws IOException, OtpAuthException, OtpErlangExit {

        if (otpConnection.isConnected()) {
            otpConnection.sendRPC("master_supervisor", "terminate", new OtpErlangList(supervisorPid));
            otpConnection.receiveRPC();
        }

        otpConnection.close();
        erlangProcess.destroy();

    }


}
