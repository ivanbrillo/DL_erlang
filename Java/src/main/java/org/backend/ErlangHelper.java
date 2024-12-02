package org.backend;


import java.io.IOException;

public class ErlangHelper {

    public static Process startErlangNode(String path, String cookie, String name, long timeout) throws IOException, InterruptedException {
        ProcessBuilder builder = new ProcessBuilder("rebar3", "shell", "--sname", name, "--setcookie", cookie);
        builder.directory(new java.io.File(path));  // Set the working directory
        builder.environment().put("TF_CPP_MIN_LOG_LEVEL", "3");   // suppress tf info/warning messages

        // Redirect output and errors from erlang to the java console
        builder.redirectOutput(ProcessBuilder.Redirect.INHERIT);
        builder.redirectError(ProcessBuilder.Redirect.INHERIT);

        Process process = builder.start();

        System.out.println("[Java] Erlang Node compiling and starting...");
        Thread.sleep(timeout);

        if (!process.isAlive()) {
            throw new RuntimeException("[Java] Erlang node did not start correctly in 10 seconds.");
        }

        return process;
    }

}
