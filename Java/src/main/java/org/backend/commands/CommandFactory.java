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

    private Map<String, String> parseCommand(String commandJSON) {
        try {
            return objectMapper.readValue(commandJSON, new TypeReference<>() {
            });
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Command discarded with reason: " + e.getMessage());
        }
    }

    public Command createCommand(String commandJSON) throws IllegalArgumentException {

        Map<String, String> map = parseCommand(commandJSON);

        return switch (map.get("command")) {
            case "start" -> context.getBean(StartCommand.class);
            case "stop" -> context.getBean(StopCommand.class);
            case "save" -> context.getBean(SaveCommand.class);
            case "load" -> context.getBean(LoadCommand.class);
            case "stop_training" -> context.getBean(StopTrainingCommand.class);
            case "train" -> {
                TrainCommand train = context.getBean(TrainCommand.class);
                train.setParameters(map.get("parameters"));
                yield train;
            }
            default -> throw new IllegalArgumentException("Unknown command: " + map.get("command"));
        };
    }


}
