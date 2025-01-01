package org.backend.websocket;

import jakarta.annotation.PreDestroy;
import org.backend.MessageQueues;
import org.backend.erlang.ErlangContext;
import org.springframework.lang.NonNull;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.io.IOException;


@Component
public class ErlangWebSocketHandler extends TextWebSocketHandler {
    private final SessionRegistry sessionRegistry;
    private final Thread erlangMessageForwarder;
    private final MessageQueues queues;
    private final ErlangContext erlangContext;


    public ErlangWebSocketHandler(WebSocketListener webSocketListener, SessionRegistry sessionRegistry, MessageQueues queues, ErlangContext erlangContext) {
        this.queues = queues;
        this.sessionRegistry = sessionRegistry;
        this.erlangContext = erlangContext;
        erlangMessageForwarder = new Thread(webSocketListener);
        erlangMessageForwarder.start();
    }

    @Override
    public void afterConnectionEstablished(@NonNull WebSocketSession session) {

        sessionRegistry.addSession(session);
        String role = getRoleString(session);

        if (role != null && role.equals("ADMIN")) {
            try {
                session.sendMessage(new TextMessage("{operator}"));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        if (erlangContext.isConnected())
            queues.restoreSession();

        System.out.println("[SockJS] New session connected: " + session.getId());
    }

    @Override
    protected void handleTextMessage(@NonNull WebSocketSession session, TextMessage message) throws Exception {

        String role = getRoleString(session);

        if (role == null || !role.equals("ADMIN")) {
            // TODO log
            System.out.println("Received unauthorized message: " + message.getPayload());
        }

        String receivedMessage = message.getPayload();
        queues.addWebSocketMessage(receivedMessage);
        System.out.println("[SockJS] Received message: " + receivedMessage);
    }

    private static String getRoleString(WebSocketSession session) {
        SecurityContext securityContext = (SecurityContext) session.getAttributes().get("SPRING_SECURITY_CONTEXT");
        Authentication authentication = securityContext.getAuthentication();

        if (authentication == null)
            return null;

        return authentication.getAuthorities().stream()
                .map(grantedAuthority -> grantedAuthority.getAuthority().replace("ROLE_", "")) // Remove the "ROLE_" prefix
                .findFirst()
                .orElse(null);
    }

    @Override
    public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull CloseStatus status) {
        sessionRegistry.removeSession(session);
        System.out.println("[SockJS] Session closed: " + session.getId() + ", Status: " + status);
    }

    @PreDestroy
    public void cleanup() throws InterruptedException {
        erlangMessageForwarder.interrupt();
        erlangMessageForwarder.join();
    }

}