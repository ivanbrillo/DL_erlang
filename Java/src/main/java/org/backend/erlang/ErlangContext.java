package org.backend.erlang;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangPid;


public class ErlangContext {

    public OtpConnection otpConnection;
    public OtpErlangPid javaPid;

    public Thread erlangControllerThread;
    public Process erlangProcess;

    public boolean isConnected() {
        return otpConnection != null && otpConnection.isConnected();
    }

}
