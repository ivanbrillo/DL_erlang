package org.backend.erlang;

import com.ericsson.otp.erlang.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.backend.MessageQueues;
import org.backend.commands.Command;
import org.backend.commands.StopCommand;
import org.backend.commands.CommandFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Map;


@Component
public class ErlangController implements Runnable {

    private final CommandFactory commandFactory;
    private final ApplicationContext appContext;
    private final MessageQueues queues;
    private final ErlangContext erlangContext;
    private final ObjectMapper objectMapper = new ObjectMapper();

    public ErlangController(CommandFactory commandFactory, ApplicationContext appContext, MessageQueues queues, ErlangContext erlangContext) {
        this.commandFactory = commandFactory;
        this.appContext = appContext;
        this.queues = queues;
        this.erlangContext = erlangContext;
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

                if (!msg.toString().startsWith("{rex,")) {   // RPC return value, will be discarded
                    queues.addErlangMessage(msg.toString());

                    //TODO check this line
                    if (msg.toString().equals("{train_refused}") || msg.toString().startsWith("{training_total_completed,") || msg.toString().startsWith("{train_error,"))
                        erlangContext.setTraining(false);
                }
            }
        } catch (OtpAuthException | OtpErlangExit | IOException | InterruptedException e) {
            System.out.println("Message discarded from Erlang with reason: " + e.getMessage());
        }
    }

    private void executeWebSocketCommand() {
        String commandJSON = queues.getWebSocketMessage();
        if (commandJSON == null)
            return;

        try {
            Map<String, String> commandMap = parseCommand(commandJSON);
            Command command = commandFactory.createCommand(commandMap.get("command"));
            command.execute(erlangContext, commandMap.get("parameters"));
        } catch (RuntimeException e) {
            System.out.println("Cannot execute command for reason: " + e.getMessage());
        }
    }

    @PreDestroy
    public void cleanup() throws InterruptedException {
        if (erlangContext.getErlangControllerThread().isAlive()) {
            erlangContext.getErlangControllerThread().interrupt();
            erlangContext.getErlangControllerThread().join();
            try {
                appContext.getBean(StopCommand.class).execute(erlangContext, "");
            } catch (RuntimeException ignored) {
            }
        }
        System.out.println("ErlangController shutdown complete.");
    }

    private Map<String, String> parseCommand(String commandJSON) {
        try {
            return objectMapper.readValue(commandJSON, new TypeReference<>() {
            });
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Command discarded with reason: " + e.getMessage());
        }
    }
}
