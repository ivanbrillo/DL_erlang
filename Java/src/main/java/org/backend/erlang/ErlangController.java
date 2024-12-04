package org.backend.erlang;

import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.backend.MessageQueues;
import org.backend.commands.Command;
import org.backend.commands.StopCommand;
import org.backend.commands.CommandFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;


@Component
public class ErlangController implements Runnable {

    private final ErlangContext erlangContext = new ErlangContext();

    @PostConstruct
    public void init() {
        erlangContext.erlangControllerThread = new Thread(this);
        erlangContext.erlangControllerThread.start();
    }

    @Override
    public void run() {
        // TODO mettere tutto asynch in erlang
        while (!Thread.currentThread().isInterrupted()) {
            receiveErlangMessage();
            executeWebSocketCommand();
        }
    }

    private void receiveErlangMessage() {
        try {
            if (erlangContext.isConnected() && erlangContext.otpConnection.msgCount() > 0) {
                OtpErlangObject msg = erlangContext.otpConnection.receive();

                if (!msg.toString().startsWith("{rex,{")) {   // otherwise RPC return value
                    MessageQueues.erlangQueue.put(msg.toString());
                    System.out.println("Message received from Erlang: " + msg);
                }
            }
        } catch (OtpAuthException | OtpErlangExit | IOException | InterruptedException e) {
            System.out.println("Message discarded from Erlang with reason: " + e.getMessage());
        }
    }

    private void executeWebSocketCommand() {
        String commandJSON = MessageQueues.webSocketQueue.poll();
        try {
            if (commandJSON != null) {
                Command command = CommandFactory.createCommand(commandJSON);
                command.execute(erlangContext);
            }
        } catch (RuntimeException e) {
            System.out.println("Cannot execute command for reason: " + e.getMessage());
        }
    }

    @PreDestroy
    public void cleanup() throws InterruptedException, OtpAuthException, OtpErlangExit, IOException {
        if (erlangContext.erlangControllerThread.isAlive()) {
            erlangContext.erlangControllerThread.interrupt();
            erlangContext.erlangControllerThread.join();
            try {
                new StopCommand().execute(erlangContext);
            } catch (RuntimeException ignored) {
            }
        }
        System.out.println("ErlangController shutdown complete.");
    }
}
