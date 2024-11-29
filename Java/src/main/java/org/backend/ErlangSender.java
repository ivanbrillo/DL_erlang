package org.backend;

import com.ericsson.otp.erlang.*;
import java.io.IOException;


public class ErlangSender implements Runnable {
    private final OtpConnection otpConnection;

    public ErlangSender(String cookie, String javaNodeName, String erlangNodeName) throws IOException, OtpAuthException {
        OtpSelf self = new OtpSelf(javaNodeName, cookie);
        OtpPeer other = new OtpPeer(erlangNodeName);
        this.otpConnection = self.connect(other);
    }

    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                String command = MessageQueues.webSocketQueue.take();
                // execute(command);  will use otpConnection
            } catch (InterruptedException e) {
                System.out.println("[JAVA] Erlang message discard, reason: " + e.getMessage());
            }

        }
        otpConnection.close();
    }
}


