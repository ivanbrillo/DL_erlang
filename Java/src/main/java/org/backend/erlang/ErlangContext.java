package org.backend.erlang;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangPid;
import lombok.Data;


@Data
public class ErlangContext {

    private OtpConnection otpConnection;
    private OtpErlangPid javaPid;

    private Thread erlangControllerThread;
    private Process erlangProcess;

    public boolean isConnected() {
        return otpConnection != null && otpConnection.isConnected();
    }

}
