package org.backend.commands;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Map;

public class CommandFactory {

    public static Command createCommand(String commandJSON) throws IllegalArgumentException {

        ObjectMapper objectMapper = new ObjectMapper();
        Map<String, String> map;

        try {
            map = objectMapper.readValue(commandJSON, new TypeReference<Map<String, String>>() {});
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Command discarded with reason: " + e.getMessage());
        }

        return switch (map.get("command")) {
            case "start" -> new StartCommand();
            case "stop" -> new StopCommand();
            case "train" -> new TrainCommand(map.get("parameters"));
            default -> throw new IllegalArgumentException("Unknown command: " + map.get("command"));
        };

    }


}
