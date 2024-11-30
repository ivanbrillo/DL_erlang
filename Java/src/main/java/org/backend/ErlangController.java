package org.backend;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.concurrent.TimeUnit;


public class ErlangController implements Runnable {

    //    private Thread erlangSenderThread;
    private final String cookie = "cookie";
    private final String javaNodeName = "java_node";
    private final String erlangNodeName = "master@localhost";

    private OtpConnection otpConnection;
    private OtpErlangObject supervisorPid;
    private OtpErlangPid javaPid;

    @Override
    public void run() {

        try {

            // TODO remove everything -> start from web socket msg

            startErlangMaster();
            OtpSelf self = new OtpSelf(javaNodeName, cookie);
            OtpPeer other = new OtpPeer(erlangNodeName);
            this.otpConnection = self.connect(other);
            this.javaPid = self.pid();


            otpConnection.sendRPC("master_supervisor", "start_link_shell", new OtpErlangList(javaPid));
            supervisorPid = otpConnection.receiveRPC();
            System.out.println("Result: " + supervisorPid);

        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        while (!Thread.currentThread().isInterrupted()) {
            try {

                if (otpConnection.msgCount() > 0) {
                    OtpErlangObject msg = otpConnection.receive(50);
                    MessageQueues.erlangQueue.put(msg.toString());
                    System.out.println("Result: " + msg);
                }

                String command = MessageQueues.webSocketQueue.poll(50, TimeUnit.MILLISECONDS);
                // execute(command);  will use otpConnection

            } catch (Exception e) {   // TODO better handle exception
                System.out.println("[JAVA] Erlang message discard, reason: " + e.getMessage());
            }
        }

        otpConnection.close();

    }

    public void startErlangMaster() throws IOException, InterruptedException {
        String beamPath = System.getProperty("user.dir") + "/../Erlang";
        ErlangHelper.startErlangNode(beamPath, cookie, erlangNodeName, 10000);
    }

//    public void stopErlangMaster() throws InterruptedException {
////        synchronized (MessageQueues.connectionLock) {
////            otpConnection.sendRPC("master_supervisor", "terminate", new OtpErlangList(supervisorPid));
////            otpConnection.receiveRPC();
////        }
//
//        erlangSenderThread.join();   // will auto-interrupt when a close message is found
////        erlangListenerThread.interrupt();
////        erlangListenerThread.join();
//
////        javaNode.close();
//    }

//    public void createConnection() throws IOException, OtpAuthException, InterruptedException {
////        javaNode = new OtpNode(javaNodeName, cookie);
//        startErlangMaster();   // TODO to be removed or not because i need to create the connection
//        // TODO cannot compile otherwise i'm not ready in 10s
//
////        ErlangListener listener = new ErlangListener(javaNode);
////        OtpErlangPid jMailboxPid = listener.getPidMBox();
//
////        erlangListenerThread = new Thread(listener);
////        erlangListenerThread.start();
//
//        erlangSenderThread = new Thread(new ErlangSender(cookie, javaNodeName, erlangNodeName));
//        erlangSenderThread.start();
//
//    }


}
