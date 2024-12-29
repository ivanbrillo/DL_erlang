package org.backend.commands;

import com.ericsson.otp.erlang.OtpErlangObject;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;
import org.springframework.stereotype.Component;


@Component
public class SaveCommand implements Command {

    @Override
    public void execute(ErlangContext context) throws RuntimeException {

        if (!context.isConnected())
            throw new RuntimeException("Erlang process is not connected so the model cannot be saved");

        ErlangHelper.call(context.getOtpConnection(), new OtpErlangObject[]{}, "master_api", "save_model");

    }
}
