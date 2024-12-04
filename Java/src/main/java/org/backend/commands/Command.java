package org.backend.commands;

import org.backend.erlang.ErlangContext;

public interface Command {

    void execute(ErlangContext context) throws RuntimeException;

}
