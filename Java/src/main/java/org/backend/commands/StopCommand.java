package org.backend.commands;

import com.ericsson.otp.erlang.OtpErlangObject;
import org.backend.MessageQueues;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Component
public class StopCommand implements Command {

    @Autowired
    private MessageQueues queues;
    @Autowired
    ErlangContext context;

    @Override
    public void execute(String parameters) throws RuntimeException {

        if (!context.isConnected())
            throw new RuntimeException("Erlang process is not connected so it cannot be stopped");

        ErlangHelper.call(context.getOtpConnection(), new OtpErlangObject[]{}, "master_supervisor", "terminate");
        context.setTraining(false);
        queues.clearCache();
        context.getOtpConnection().close();
        context.getErlangProcess().destroy();
    }
}
