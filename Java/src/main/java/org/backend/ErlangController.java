package org.backend;

import com.ericsson.otp.erlang.*;
import java.io.IOException;


public class ErlangController {

    private OtpNode javaNode;
    private Thread erlangListenerThread;
    private Thread erlangSenderThread;

    private final String cookie = "cookie";
    private final String javaNodeName = "java_node";
    private final String erlangNodeName = "master@localhost";


//    public void startErlangMaster() throws IOException, InterruptedException {
//        String beamPath = System.getProperty("user.dir") + "/../Erlang";
//        ErlangHelper.startErlangNode(beamPath, cookie, erlangNodeName, 10000);
//    }

    public void stopErlangMaster() throws InterruptedException {
//        synchronized (MessageQueues.connectionLock) {
//            otpConnection.sendRPC("master_supervisor", "terminate", new OtpErlangList(supervisorPid));
//            otpConnection.receiveRPC();
//        }

        erlangSenderThread.join();   // will auto-interrupt when a close message is found
        erlangListenerThread.interrupt();
        erlangListenerThread.join();

        javaNode.close();
    }

    public void createConnection() throws IOException, OtpAuthException {
        javaNode = new OtpNode(javaNodeName, cookie);

        erlangListenerThread = new Thread(new ErlangListener(javaNode));
        erlangListenerThread.start();

        erlangSenderThread = new Thread(new ErlangSender(cookie, javaNodeName, erlangNodeName));
        erlangSenderThread.start();
    }

//    public void setupErlangMaster() throws IOException, OtpAuthException, OtpErlangExit {
//            otpConnection.sendRPC("master_supervisor", "start_link_shell", new OtpErlangList());
//            supervisorPid = otpConnection.receiveRPC();
//    }


}
