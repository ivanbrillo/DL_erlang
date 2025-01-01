package org.backend.commands;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadCommand implements Command {

    @Autowired
    ErlangContext context;

    @Override
    public void execute(String parameters) throws RuntimeException {

        if (!context.isConnected())
            throw new RuntimeException("Erlang process is not connected so the model cannot be loaded");

        ErlangHelper.call(context.getOtpConnection(), new OtpErlangObject[]{
                new OtpErlangAtom(parameters),
        }, "master_api", "load_model");

    }
}
