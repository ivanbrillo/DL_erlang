package org.backend.websocket;

import jakarta.annotation.PreDestroy;
import org.backend.MessageQueues;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@Component
public class ErlangWebSocketHandler extends TextWebSocketHandler {
    private final List<WebSocketSession> sessions = new CopyOnWriteArrayList<>();
    private final Thread erlangMessageForwarder = new Thread(new WebSocketListener(sessions));

    public ErlangWebSocketHandler() {
        erlangMessageForwarder.start();
    }

    @Override
    public void afterConnectionEstablished(@NonNull WebSocketSession session) {
        sessions.add(session);
        System.out.println("[SockJS] New session connected: " + session.getId());
    }

    @Override
    protected void handleTextMessage(@NonNull WebSocketSession session, TextMessage message) throws Exception {
        String receivedMessage = message.getPayload();
        MessageQueues.webSocketQueue.put(receivedMessage);
        System.out.println("[SockJS] Received message: " + receivedMessage);
    }

    @Override
    public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull org.springframework.web.socket.CloseStatus status) {
        sessions.remove(session);
        System.out.println("[SockJS] Session closed: " + session.getId() + ", Status: " + status);
    }

    @PreDestroy
    public void cleanup() throws InterruptedException {
        erlangMessageForwarder.interrupt();
        erlangMessageForwarder.join();
    }
}