package org.backend.websocket;

import jakarta.annotation.PreDestroy;
import org.backend.MessageQueues;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;


@Component
public class ErlangWebSocketHandler extends TextWebSocketHandler {
    private final SessionRegistry sessionRegistry;
    private final Thread erlangMessageForwarder;
    private final MessageQueues queues;

    public ErlangWebSocketHandler(WebSocketListener webSocketListener, SessionRegistry sessionRegistry, MessageQueues queues) {
        this.queues = queues;
        this.sessionRegistry = sessionRegistry;
        erlangMessageForwarder = new Thread(webSocketListener);
        erlangMessageForwarder.start();
    }

    @Override
    public void afterConnectionEstablished(@NonNull WebSocketSession session) {
        sessionRegistry.getSessions().add(session);
        System.out.println("[SockJS] New session connected: " + session.getId());
    }

    @Override
    protected void handleTextMessage(@NonNull WebSocketSession session, TextMessage message) throws Exception {
        String receivedMessage = message.getPayload();
        queues.addWebSocketMessage(receivedMessage);
        System.out.println("[SockJS] Received message: " + receivedMessage);
    }

    @Override
    public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull CloseStatus status) {
        sessionRegistry.getSessions().remove(session);
        System.out.println("[SockJS] Session closed: " + session.getId() + ", Status: " + status);
    }

    @PreDestroy
    public void cleanup() throws InterruptedException {
        erlangMessageForwarder.interrupt();
        erlangMessageForwarder.join();
    }

}