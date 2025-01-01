package org.backend.erlang;

import com.ericsson.otp.erlang.*;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.backend.MessageQueues;
import org.backend.commands.Command;
import org.backend.commands.CommandFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.List;
import java.util.Map;


@Component
public class ErlangController implements Runnable {

    private final CommandFactory commandFactory;
    private final MessageQueues queues;
    private final ErlangContext erlangContext;
    private final ObjectMapper objectMapper = new ObjectMapper();

    private final List<String> finishedTrainCode = List.of("train_refused", "training_total_completed", "train_error");


    public ErlangController(CommandFactory commandFactory, MessageQueues queues, ErlangContext erlangContext) {
        this.commandFactory = commandFactory;
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
                String msg = erlangContext.getOtpConnection().receive().toString();

                if (!msg.startsWith("{rex,")) {   // RPC return value, will be discarded
                    queues.addErlangMessage(msg);

                    if (finishedTrainCode.stream().anyMatch(code -> msg.startsWith("{" + code)))
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
            command.execute(commandMap.get("parameters"));
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
                commandFactory.createCommand("stop").execute("");
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
