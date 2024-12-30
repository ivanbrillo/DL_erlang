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

    private final ApplicationContext context;

    @Autowired
    public CommandFactory(ApplicationContext context) {
        this.context = context;
    }
    
    public Command createCommand(String commandCode) throws IllegalArgumentException {
        return switch (commandCode) {
            case "start" -> context.getBean(StartCommand.class);
            case "stop" -> context.getBean(StopCommand.class);
            case "save" -> context.getBean(SaveCommand.class);
            case "load" -> context.getBean(LoadCommand.class);
            case "stop_training" -> context.getBean(StopTrainingCommand.class);
            case "train" -> context.getBean(TrainCommand.class);
            default -> throw new IllegalArgumentException("Unknown command: " + commandCode);
        };
    }


}
