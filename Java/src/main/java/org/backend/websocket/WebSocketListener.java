package org.backend.websocket;

import org.backend.MessageQueues;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Component
public class WebSocketListener implements Runnable {

    private final SessionRegistry sessionRegistry;
    private final MessageQueues queues;

    @Autowired
    public WebSocketListener(SessionRegistry sessionRegistry, MessageQueues queues) {
        this.sessionRegistry = sessionRegistry;
        this.queues = queues;
    }


    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            try {

                String erlangMessage = queues.getErlangMessage();   // blocking call

                if (!erlangMessage.startsWith("{node_metrics"))
                    System.out.println("[WebSocket] Send erlang message to active sessions " + erlangMessage);

                // Broadcast to all active WebSocket sessions
                sessionRegistry.broadcastMessage(erlangMessage);

            } catch (InterruptedException e) {
                System.err.println("[WebSocket] Thread interrupted during take()");
                Thread.currentThread().interrupt();   // restore the flag
            } catch (IOException e) {
                System.err.println("[WebSocket] Error sending Erlang message: " + e.getMessage());
            }
        }
    }
}
