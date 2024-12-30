package org.backend.websocket;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketSession;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@Component
public class SessionRegistry {
    private final List<WebSocketSession> sessions = new CopyOnWriteArrayList<>();

    public List<WebSocketSession> getSessions() {
        return sessions;
    }
}
