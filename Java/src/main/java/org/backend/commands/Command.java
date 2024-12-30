package org.backend.commands;

import org.backend.erlang.ErlangContext;

public interface Command {

    void execute(ErlangContext context, String parameters) throws RuntimeException;

}
