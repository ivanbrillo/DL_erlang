package org.backend.commands;


public interface Command {

    void execute(String parameters) throws RuntimeException;

}
