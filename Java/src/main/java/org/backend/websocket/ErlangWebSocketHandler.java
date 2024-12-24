package org.backend.websocket;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.JwtException;
import jakarta.annotation.PreDestroy;
import org.backend.MessageQueues;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@Component
public class ErlangWebSocketHandler extends TextWebSocketHandler {
    private final List<WebSocketSession> sessions = new CopyOnWriteArrayList<>();   // thread-safe structure for add and remove
    private final Thread erlangMessageForwarder = new Thread(new WebSocketListener(sessions));

    // Start message forwarder when handler is initialized
    public ErlangWebSocketHandler() {
        erlangMessageForwarder.start();
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {

        String token = session.getUri().getQuery().split("=")[1];

        if (token == null || !isValidToken(token)) {
            session.close(CloseStatus.BAD_DATA);
        } else{
            sessions.add(session);
            System.out.println("[WebSocket] New session connected: " + session.getId());
        }
    }

    private boolean isValidToken(String token) {
        try {
            Jwts.parser().setSigningKey("secret").parseClaimsJws(token);
            return true;
        } catch (JwtException e) {
            return false;
        }
    }

    @Override
    protected void handleTextMessage(@NonNull WebSocketSession session, TextMessage message) throws Exception {
        String receivedMessage = message.getPayload();
        MessageQueues.webSocketQueue.put(receivedMessage);
        System.out.println("[WebSocket] Received message: " + receivedMessage);
    }

    @Override
    public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull org.springframework.web.socket.CloseStatus status) {
        sessions.remove(session);
        System.out.println("[WebSocket] Session closed: " + session.getId() + ", Status: " + status);
    }

    @PreDestroy
    public void cleanup() throws InterruptedException {
        erlangMessageForwarder.interrupt();
        erlangMessageForwarder.join();
    }

}