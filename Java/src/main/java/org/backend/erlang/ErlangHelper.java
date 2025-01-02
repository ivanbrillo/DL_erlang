package org.backend.erlang;


import com.ericsson.otp.erlang.*;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.IOException;

@Slf4j
public class ErlangHelper {

    public static Process startErlangNode(String path, String cookie, String name, long timeout) throws IOException, InterruptedException, RuntimeException {
        ProcessBuilder builder = new ProcessBuilder("rebar3", "shell", "--sname", name, "--setcookie", cookie);
        builder.directory(new java.io.File(path));  // Set the working directory
        builder.environment().put("TF_CPP_MIN_LOG_LEVEL", "3");   // suppress tf info/warning messages

        // Redirect output and errors from erlang to the java console and to log file
        builder.redirectOutput(ProcessBuilder.Redirect.INHERIT);
        builder.redirectOutput(ProcessBuilder.Redirect.appendTo(new File("logs/erlang.log")));
        builder.redirectError(ProcessBuilder.Redirect.INHERIT);

        Process process = builder.start();

        log.info("[Java] Erlang Node compiling and starting");
        Thread.sleep(timeout);

        if (!process.isAlive()) {
            process.destroyForcibly();
            throw new RuntimeException("[Java] Erlang node did not start correctly in 10 seconds.");
        }

        log.info("[Java] Erlang Node compiled and started");
        return process;
    }

    public static void call(OtpConnection otpConnection, OtpErlangObject[] parameters, String module, String methodName) throws RuntimeException {
        try {
            otpConnection.sendRPC(module, methodName, new OtpErlangList(parameters));
        } catch (IOException e) {
            throw new RuntimeException("Cannot perform correctly the call", e);
        }
    }

}
