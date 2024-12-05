package org.backend.commands;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.context.ApplicationContext;

import java.util.Map;

@Component
public class CommandFactory {

    private final ObjectMapper objectMapper = new ObjectMapper();
    private final ApplicationContext context;

    @Autowired
    public CommandFactory(ApplicationContext context) {
        this.context = context;
    }

    public Command createCommand(String commandJSON) throws IllegalArgumentException {

        Map<String, String> map;

        try {
            map = objectMapper.readValue(commandJSON, new TypeReference<Map<String, String>>() {
            });
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Command discarded with reason: " + e.getMessage());
        }

        return switch (map.get("command")) {
            case "start" -> context.getBean(StartCommand.class);
            case "stop" -> context.getBean(StopCommand.class);
            case "train" -> new TrainCommand(map.get("parameters"));  // custom parameters, so it cannot be a Bean
            default -> throw new IllegalArgumentException("Unknown command: " + map.get("command"));
        };

    }


}
