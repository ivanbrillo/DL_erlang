package org.backend;

import com.ericsson.otp.erlang.*;


public class ErlangListener implements Runnable {
    private final OtpMbox otpMbox;


    public ErlangListener(OtpNode javaNode) {
        this.otpMbox = javaNode.createMbox();
    }

    public OtpErlangPid getPidMBox() {
        return otpMbox.self();
    }

    @Override
    public void run() {
        System.out.println("reading1");   // TODO remove

        System.err.println(otpMbox.self().toString());
//        OtpErlangTuple request = createMessage(otpMbox, "master", "acuaaa");
//        otpMbox.send("master", "master@localhost", request);


        while (!Thread.currentThread().isInterrupted()) {
            try {
                System.out.println("reading2");   // TODO remove

                OtpErlangObject msg = otpMbox.receive();
                MessageQueues.erlangQueue.put(msg.toString());
                System.out.println("[JAVA] Received message: " + msg);
            } catch (Exception e) {
                System.out.println("[JAVA] Erlang message discard, reason: " + e.getMessage());
            }
        }
    }

    private static OtpErlangTuple createMessage(OtpMbox mbox, String nodeName, String command) {  // TODO remove
        return new OtpErlangTuple(new OtpErlangObject[] {
                mbox.self(),
                new OtpErlangTuple(new OtpErlangObject[] {
                        new OtpErlangAtom(nodeName), // Nome del modulo Erlang
                        new OtpErlangAtom(command) // Messaggio di start
                })
        });
    }

}

