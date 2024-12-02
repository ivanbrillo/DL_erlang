package org.backend;


import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Command {
    public String command;
    public String parameters;

    public String getParamValue(String parametersName) {
        Pattern pattern = Pattern.compile(parametersName + "=([^,]+)");
        Matcher matcher = pattern.matcher(parameters);

        if (matcher.find())
            return matcher.group(1);

        return null; // or you could return a default value
    }

    public int getTrainEpochs() {
        String epochs = getParamValue("epochs");
        return Integer.parseInt(epochs);
    }

    public double getAccuracy() {
        String epochs = getParamValue("accuracy");
        return Double.parseDouble(epochs);
    }


    public boolean isValid() {
        return command != null && parameters != null;
    }

    // TODO make an interface command with is valid and then specialize

    @Override
    public String toString() {
        return "Command [ command=" + command + ", parameters=" + parameters + " ]";
    }

}

