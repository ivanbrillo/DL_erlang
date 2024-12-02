package org.backend.commands;

import org.backend.ErlangContext;

public interface Command {

    void execute(ErlangContext context) throws RuntimeException;

}
