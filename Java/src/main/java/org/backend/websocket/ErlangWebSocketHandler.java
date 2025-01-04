package org.backend.websocket;

import jakarta.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
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


@Slf4j
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

        if (erlangContext.isConnected()) {   
            try {
                queues.restoreSession(session);   // show current state of executions to new client
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.error("Session restoration interrupted");
                throw new RuntimeException("Session restoration interrupted", e);
            } catch (RuntimeException e) {
                log.error("Error restoring session {}", e.getMessage());
                return;
            }
        }

        sessionRegistry.addSession(session);
        String role = getRoleString(session);

        if (role != null && role.equals("ADMIN")) {
            try {
                session.sendMessage(new TextMessage("{operator}"));  // show buttons on the UI
            } catch (IOException e) {
                log.error("Error enabling admin command in UI: {}", e.getMessage());
                throw new RuntimeException(e);
            }
        }

        log.info("[SockJS] New session connected: {}", session.getId());

    }

    @Override
    protected void handleTextMessage(@NonNull WebSocketSession session, TextMessage message) throws Exception {

        String role = getRoleString(session);

        if (role == null || !role.equals("ADMIN")) {
            log.warn("[SockJS] Received unauthorized message: {}, from session: {}", message.getPayload(), session.getId());
            return;
        }

        String receivedMessage = message.getPayload();
        queues.addWebSocketMessage(receivedMessage);
        log.info("[SockJS] Received message: {}", receivedMessage);

    }

    private static String getRoleString(WebSocketSession session) {
        SecurityContext securityContext = (SecurityContext) session.getAttributes().get("SPRING_SECURITY_CONTEXT");
        Authentication authentication = securityContext.getAuthentication();

        return authentication == null ? null : authentication.getAuthorities().stream()
                .map(grantedAuthority -> grantedAuthority.getAuthority().replace("ROLE_", "")) // Remove the "ROLE_" prefix
                .findFirst()
                .orElse(null);

    }

    @Override
    public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull CloseStatus status) {
        sessionRegistry.removeSession(session);
        log.info("[SockJS] Session closed: {} with status: {}", session.getId(), status);
    }

    @PreDestroy
    public void cleanup() throws InterruptedException {
        erlangMessageForwarder.interrupt();
        erlangMessageForwarder.join();
    }

}