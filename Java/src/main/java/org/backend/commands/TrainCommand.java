package org.backend.commands;


import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TrainCommand implements Command {

    int epochs;
    double targetAccuracy;

    public TrainCommand(String parameters) {
        try {
            epochs = Integer.parseInt(Objects.requireNonNull(getParamValue(parameters, "epochs")));
            targetAccuracy = Double.parseDouble(Objects.requireNonNull(getParamValue(parameters, "targetAccuracy")));
        } catch (NumberFormatException | NullPointerException e) {
            throw new IllegalArgumentException("Train Command discarded for bad arguments", e);
        }
    }

    private String getParamValue(String parameters, String parametersName) {
        Pattern pattern = Pattern.compile(parametersName + "=([^,]+)");
        Matcher matcher = pattern.matcher(parameters);

        if (matcher.find())
            return matcher.group(1);

        return null;
    }
    
    @Override
    public void execute(ErlangContext context) throws RuntimeException {
        if (!context.isConnected())
            throw new RuntimeException("Erlang process is not connected, unable to perform the train");

        ErlangHelper.call(context.otpConnection, new OtpErlangObject[]{
                new OtpErlangInt(epochs),
                new OtpErlangDouble(targetAccuracy)
        }, "master_api", "train");
    }

}

