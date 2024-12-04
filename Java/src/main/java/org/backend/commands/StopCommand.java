package org.backend.commands;

import com.ericsson.otp.erlang.OtpErlangObject;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;

public class StopCommand implements Command {

    @Override
    public void execute(ErlangContext context) throws RuntimeException {

        if (!context.isConnected())
            throw new RuntimeException("Erlang process is not connected so it cannot be stopped");

        ErlangHelper.call(context.otpConnection, new OtpErlangObject[]{}, "master_supervisor", "terminate");
        context.otpConnection.close();
        context.erlangProcess.destroy();
    }
}
