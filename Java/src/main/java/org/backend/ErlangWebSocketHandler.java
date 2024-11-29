package org.backend;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@Component
public class ErlangWebSocketHandler extends TextWebSocketHandler {
    // Keep track of active WebSocket sessions
    private final List<WebSocketSession> sessions = new CopyOnWriteArrayList<>();

    // Thread to continuously send Erlang messages to WebSocket clients
    private final Thread erlangMessageForwarder = new Thread(() -> {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                // Take message from Erlang queue (blocking call)
                String erlangMessage = MessageQueues.erlangQueue.take();

                // Broadcast to all active WebSocket sessions
                for (WebSocketSession session : sessions) {
                    if (session.isOpen()) {
                        session.sendMessage(new TextMessage(erlangMessage));
                    }
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (IOException e) {
                System.err.println("[WebSocket] Error sending Erlang message: " + e.getMessage());
            }
        }
    });

    // Start message forwarder when handler is initialized
    public ErlangWebSocketHandler() {
        erlangMessageForwarder.setDaemon(true);
        erlangMessageForwarder.start();
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        sessions.add(session);
        System.out.println("[WebSocket] New session connected: " + session.getId());
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception {
        // Add received WebSocket message to WebSocket queue for Erlang sender
        String receivedMessage = message.getPayload();
        MessageQueues.webSocketQueue.put(receivedMessage);

        System.out.println("[WebSocket] Received message: " + receivedMessage);
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, org.springframework.web.socket.CloseStatus status) {
        sessions.remove(session);
        System.out.println("[WebSocket] Session closed: " + session.getId() + ", Status: " + status);
    }
}