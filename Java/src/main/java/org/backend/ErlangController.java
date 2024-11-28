package org.backend;

import com.ericsson.otp.erlang.*;
import java.io.IOException;


public class ErlangController {

    private OtpErlangObject supervisorPid;
    private OtpNode javaNode;
    private OtpConnection otpConnection;
    private OtpMbox otpMbox;
    private final String cookie = "cookie";


    public void startErlangMaster() throws IOException, InterruptedException {
        String beamPath = System.getProperty("user.dir") + "/../Erlang";
        ErlangHelper.startErlangNode(beamPath, cookie, "master@localhost", 10000);
    }

    public void stopErlangMaster() throws IOException, OtpAuthException, OtpErlangExit {
        otpConnection.sendRPC("master_supervisor","terminate", new OtpErlangList(supervisorPid));
        otpConnection.receiveRPC();

        otpConnection.close();
        javaNode.close();
    }

    public void createConnection() throws IOException, OtpAuthException {
        javaNode = new OtpNode("java_node", cookie);
        otpMbox = javaNode.createMbox();

        OtpSelf self = new OtpSelf("java_node", cookie);
        OtpPeer other = new OtpPeer("master@localhost");
        otpConnection = self.connect(other);
    }

    public void setupErlangMaster() throws IOException, OtpAuthException, OtpErlangExit {
        otpConnection.sendRPC("master_supervisor","start_link_shell", new OtpErlangList());
        supervisorPid = otpConnection.receiveRPC();
    }


}
