package org.backend.commands;

import com.ericsson.otp.erlang.OtpErlangObject;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;
import org.springframework.stereotype.Component;

@Component
public class StopTrainingCommand implements Command {

    @Override
    public void execute(ErlangContext context) throws RuntimeException {

        if (!context.isConnected() || !context.isTraining())
            throw new RuntimeException("Erlang process is not connected or in training so i cannot stop the training");

        ErlangHelper.call(context.getOtpConnection(), new OtpErlangObject[]{}, "master_api", "save_model");

    }
}
