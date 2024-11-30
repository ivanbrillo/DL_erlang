package org.backend;

import com.ericsson.otp.erlang.*;
import java.io.IOException;


public class ErlangSender implements Runnable {
    private final OtpConnection otpConnection;
    OtpErlangObject supervisorPid;
    OtpErlangPid jMailboxPid;

    public ErlangSender(String cookie, String javaNodeName, String erlangNodeName) throws IOException, OtpAuthException {
        OtpSelf self = new OtpSelf(javaNodeName, cookie);
        OtpPeer other = new OtpPeer(erlangNodeName);
        this.otpConnection = self.connect(other);
        this.jMailboxPid = self.pid();
    }

    @Override
    public void run() {

        try {

            System.out.println("Starting master with all the functions");  // TODO to be removed
            otpConnection.sendRPC("master_supervisor", "start_link_shell", new OtpErlangList(jMailboxPid));
            supervisorPid = otpConnection.receiveRPC();
            System.out.println("Result: " + supervisorPid);

            otpConnection.sendRPC("master_api", "get_nodes", new OtpErlangList());
            supervisorPid = otpConnection.receiveRPC();
            System.out.println("Result: " + supervisorPid);

            otpConnection.sendRPC("master_api", "get_server_pid", new OtpErlangList());
            supervisorPid = otpConnection.receiveRPC();
            System.out.println("Result: " + supervisorPid);

            int i = 100;
            while(i > 0) {
                supervisorPid = otpConnection.receive();
                System.out.println("Result: " + supervisorPid);
                i--;
            }


        } catch (Exception e) {
            throw new RuntimeException(e);
        }


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
