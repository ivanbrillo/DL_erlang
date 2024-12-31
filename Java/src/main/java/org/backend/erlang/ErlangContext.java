package org.backend.erlang;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangPid;
import lombok.Data;
import org.springframework.stereotype.Component;


@Data
@Component
public class ErlangContext {

    private OtpConnection otpConnection;
    private OtpErlangPid javaPid;

    private Thread erlangControllerThread;
    private Process erlangProcess;
    private boolean isTraining = false;

    public synchronized boolean isConnected() {
        return otpConnection != null && otpConnection.isConnected();
    }

    // Synchronized method to set the connection
    public synchronized void setOtpConnection(OtpConnection connection) {
        this.otpConnection = connection;
    }

}
