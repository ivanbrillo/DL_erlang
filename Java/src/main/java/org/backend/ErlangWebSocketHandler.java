package org.backend;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.io.EOFException;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@Component
public class ErlangWebSocketHandler extends TextWebSocketHandler {
    private final List<WebSocketSession> sessions = new CopyOnWriteArrayList<>();   // thread-safe structure, read intensive

    // Thread to continuously send Erlang messages to WebSocket clients
    private final Thread erlangMessageForwarder = new Thread(new WebSocketListener(sessions));

    // Start message forwarder when handler is initialized
    public ErlangWebSocketHandler() {
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