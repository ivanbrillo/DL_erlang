package org.backend;

import com.ericsson.otp.erlang.*;


public class ErlangListener implements Runnable {
    private final OtpMbox otpMbox;

    public ErlangListener(OtpNode javaNode) {
        this.otpMbox = javaNode.createMbox();
    }

    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                OtpErlangObject msg = otpMbox.receive();
                MessageQueues.erlangQueue.put(msg.toString());
            } catch (Exception e) {
                System.out.println("[JAVA] Erlang message discard, reason: " + e.getMessage());
            }
        }
    }
}

