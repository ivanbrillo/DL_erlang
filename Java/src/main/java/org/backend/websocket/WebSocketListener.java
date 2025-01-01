package org.backend.websocket;

import lombok.extern.slf4j.Slf4j;
import org.backend.MessageQueues;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Slf4j
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
                    log.info("[WebSocket] Send erlang message to active sessions {}", erlangMessage);

                // Broadcast to all active WebSocket sessions
                sessionRegistry.broadcastMessage(erlangMessage);

            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();   // restore the flag
                log.error("[WebSocket] Thread interrupted during take()");
            } catch (IOException e) {
                log.error("[WebSocket] Error sending Erlang message: {}", e.getMessage());
            }
        }
    }
}
