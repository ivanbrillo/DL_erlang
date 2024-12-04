package org.backend.commands;

import com.ericsson.otp.erlang.*;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;
import java.io.IOException;

public class StartCommand implements Command {

    private final String cookie = "cookie";
    private final String javaNodeName = "java_node";
    private final String erlangNodeName = "master@localhost";

    @Override
    public void execute(ErlangContext context) throws RuntimeException {

        if (context.isConnected())
            throw new RuntimeException("Erlang process is already started and connected");

        System.out.println(cookie + " " + javaNodeName + " " + erlangNodeName);
        String beamPath = System.getProperty("user.dir") + "/../Erlang";
        try {
            context.erlangProcess = ErlangHelper.startErlangNode(beamPath, cookie, erlangNodeName, 10000);
        } catch (IOException | InterruptedException | RuntimeException e) {
            throw new RuntimeException("Cannot start Erlang node " + e.getMessage(), e);
        }

        try {
            OtpSelf self = new OtpSelf(javaNodeName, cookie);
            OtpPeer master = new OtpPeer(erlangNodeName);
            context.otpConnection = self.connect(master);
            context.javaPid = self.pid();
        } catch (IOException | OtpAuthException e) {
            context.erlangProcess.destroyForcibly();
            throw new RuntimeException("Cannot start correctly the connection with erlang", e);

        }

        ErlangHelper.call(context.otpConnection, new OtpErlangObject[]{context.javaPid}, "master_supervisor", "start_link_shell");

    }
}
