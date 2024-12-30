package org.backend.commands;


import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangObject;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;
import org.springframework.stereotype.Component;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


@Component
public class TrainCommand implements Command {

    private static String getParamValue(String parameters, String parametersName) {
        Pattern pattern = Pattern.compile(parametersName + "=([^,]+)");
        Matcher matcher = pattern.matcher(parameters);

        if (matcher.find())
            return matcher.group(1);

        return null;
    }

    private static double getParameter(String parameters, String parameterName) {
        try {
            return Double.parseDouble(Objects.requireNonNull(getParamValue(parameters, parameterName)));
        } catch (NumberFormatException | NullPointerException e) {
            throw new IllegalArgumentException("Train Command discarded for bad arguments", e);
        }
    }

    @Override
    public void execute(ErlangContext context, String parameters) throws RuntimeException {
        if (!context.isConnected() || context.isTraining())
            throw new RuntimeException("Erlang process is not connected or already in training, unable to perform the train");

        context.setTraining(true);
        ErlangHelper.call(context.getOtpConnection(), new OtpErlangObject[]{
                new OtpErlangInt((int)getParameter(parameters, "epochs")),
                new OtpErlangDouble(getParameter(parameters, "targetAccuracy"))
        }, "master_api", "train");
    }

}

