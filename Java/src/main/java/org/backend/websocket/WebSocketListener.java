package org.backend.websocket;

import org.backend.MessageQueues;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.List;

public class WebSocketListener implements Runnable {

    private final List<WebSocketSession> sessions;

    public WebSocketListener(List<WebSocketSession> sessions) {
        this.sessions = sessions;
    }

    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            try {

                String erlangMessage = MessageQueues.erlangQueue.take();   // blocking call
                System.out.println("[WebSocket] Send erlang message to active sessions " + erlangMessage);

                // Broadcast to all active WebSocket sessions
                for (WebSocketSession session : sessions)    // no need to syncronize since only one thread will access to a session obj
                    if (session.isOpen())
                        session.sendMessage(new TextMessage(erlangMessage));

            } catch (InterruptedException e) {
                System.err.println("[WebSocket] Thread interrupted during take()");
                Thread.currentThread().interrupt();   // restore the flag
            } catch (IOException e) {
                System.err.println("[WebSocket] Error sending Erlang message: " + e.getMessage());
            }
        }
    }
}
