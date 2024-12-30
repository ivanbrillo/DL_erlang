package org.backend.commands;

import com.ericsson.otp.erlang.OtpErlangObject;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;
import org.springframework.stereotype.Component;

@Component
public class LoadCommand implements Command {

    @Override
    public void execute(ErlangContext context, String parameters) throws RuntimeException {

        if (!context.isConnected())
            throw new RuntimeException("Erlang process is not connected so the model cannot be loaded");

        ErlangHelper.call(context.getOtpConnection(), new OtpErlangObject[]{}, "master_api", "load_model");

    }
}
