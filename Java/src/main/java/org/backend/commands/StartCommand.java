package org.backend.commands;

import com.ericsson.otp.erlang.*;
import org.backend.erlang.ErlangContext;
import org.backend.erlang.ErlangHelper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Component
public class StartCommand implements Command {

    private final String cookie;
    private final String javaNodeName;
    private final String erlangNodeName;
    private final String beamPath = System.getProperty("user.dir") + "/../Erlang";

    public StartCommand(@Value("${node.cookie}") String cookie,
                        @Value("${node.javaNodeName}") String javaNodeName,
                        @Value("${node.erlangNodeName}") String erlangNodeName) {
        this.cookie = cookie;
        this.javaNodeName = javaNodeName;
        this.erlangNodeName = erlangNodeName;
    }


    @Override
    public void execute(ErlangContext context) throws RuntimeException {

        if (context.isConnected())
            throw new RuntimeException("Erlang process is already started and connected");

        try {
            context.setErlangProcess(ErlangHelper.startErlangNode(beamPath, cookie, erlangNodeName, 10000));
        } catch (IOException | InterruptedException | RuntimeException e) {
            throw new RuntimeException("Cannot start Erlang node " + e.getMessage(), e);
        }

        try {
            OtpSelf self = new OtpSelf(javaNodeName, cookie);
            OtpPeer master = new OtpPeer(erlangNodeName);
            context.setOtpConnection(self.connect(master));
            context.setJavaPid(self.pid());
        } catch (IOException | OtpAuthException e) {
            context.getErlangProcess().destroyForcibly();
            throw new RuntimeException("Cannot start correctly the connection with erlang", e);

        }

        ErlangHelper.call(context.getOtpConnection(), new OtpErlangObject[]{context.getJavaPid()}, "master_supervisor", "start_link_shell");

    }
}
