package org.backend.erlang;


import com.ericsson.otp.erlang.*;
import lombok.extern.slf4j.Slf4j;

import java.io.*;

@Slf4j
public class ErlangHelper {

    public static Process startErlangNode(String path, String cookie, String name, long timeout) throws InterruptedException, IOException {
        ProcessBuilder builder = new ProcessBuilder("rebar3", "shell", "--sname", name, "--setcookie", cookie);
        builder.directory(new File(path));
        builder.environment().put("TF_CPP_MIN_LOG_LEVEL", "3");   // disable tf warning

        // Redirect to both file and reader
        File logFile = new File("logs/erlang.log");
        builder.redirectErrorStream(true); // Merge stdout and stderr
        Process process = builder.start();

        log.info("[Java] Erlang Node compiling and starting");

        // Start background thread to handle output
        new Thread(() -> {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
                 FileWriter writer = new FileWriter(logFile, true)) {
                String line;
                while (process.isAlive() && (line = reader.readLine()) != null) {
                    writer.write(line + "\n");
                    writer.flush();
                    System.out.println(line);
                }
                log.info("[Java] Erlang logging terminating");
            } catch (IOException e) {
                log.info("[Java] Erlang logging terminating: {}", e.getMessage());
            } 
        }).start();

        Thread.sleep(timeout);

        if (!process.isAlive()) {
            process.destroyForcibly();
            throw new RuntimeException("[Java] Erlang node did not start correctly in " + timeout + " seconds.");
        }

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
