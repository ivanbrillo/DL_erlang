package org.backend.erlang;

import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.backend.MessageQueues;
import org.backend.commands.Command;
import org.backend.commands.StopCommand;
import org.backend.commands.CommandFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.io.IOException;


@Component
public class ErlangController implements Runnable {

    private final ErlangContext erlangContext = new ErlangContext();
    private final CommandFactory commandFactory;
    private final ApplicationContext appContext;


    public ErlangController(CommandFactory commandFactory, ApplicationContext appContext) {
        this.commandFactory = commandFactory;
        this.appContext = appContext;
    }

    @PostConstruct
    public void init() {
        erlangContext.setErlangControllerThread(new Thread(this));
        erlangContext.getErlangControllerThread().start();
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
            if (erlangContext.isConnected() && erlangContext.getOtpConnection().msgCount() > 0) {
                OtpErlangObject msg = erlangContext.getOtpConnection().receive();

                if (!msg.toString().startsWith("{rex,{")) {   // otherwise RPC return value
                    MessageQueues.erlangQueue.put(msg.toString());
                    System.out.println("Message received from Erlang: " + msg);

                    if (msg.toString().startsWith("{training_total_completed,"))
                        erlangContext.setTraining(false);
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
                Command command = commandFactory.createCommand(commandJSON);
                command.execute(erlangContext);
            }
        } catch (RuntimeException e) {
            System.out.println("Cannot execute command for reason: " + e.getMessage());
        }
    }

    @PreDestroy
    public void cleanup() throws InterruptedException, OtpAuthException, OtpErlangExit, IOException {
        if (erlangContext.getErlangControllerThread().isAlive()) {
            erlangContext.getErlangControllerThread().interrupt();
            erlangContext.getErlangControllerThread().join();
            try {
                appContext.getBean(StopCommand.class).execute(erlangContext);
            } catch (RuntimeException ignored) {
            }
        }
        System.out.println("ErlangController shutdown complete.");
    }
}
