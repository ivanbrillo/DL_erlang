package org.backend.erlang;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangPid;
import lombok.Data;
import org.springframework.stereotype.Component;

import java.io.IOException;


@Data
@Component
public class ErlangContext {
    // all the attributes cannot be accessed concurrently by code, except otpConnection with isConnected()
    // in ErlangWebSocket and other methods in erlang package like otpConnection.close(), but this will not
    // create any exception or race condition. Notice that at most one command is executing in any moment

    private OtpConnection otpConnection;
    private OtpErlangPid javaPid;

    private Thread erlangControllerThread;
    private Process erlangProcess;
    private boolean isTraining = false;


    public String getNextMessage() throws OtpAuthException, OtpErlangExit, IOException {
        if(isConnected() &&  otpConnection.msgCount() > 0)
            return otpConnection.receive().toString();

        return null;
    }

    public synchronized boolean isConnected() {
        return otpConnection != null && otpConnection.isConnected();
    }

    // Synchronized method to set the connection, since in isConnected could throw NullPointer, not used in code
    public synchronized void setOtpConnection(OtpConnection connection) {
        this.otpConnection = connection;
    }

}
