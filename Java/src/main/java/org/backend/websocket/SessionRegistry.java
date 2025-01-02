package org.backend.websocket;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

@Component
public class SessionRegistry {
    private final List<WebSocketSession> sessions = new CopyOnWriteArrayList<>();

    public void broadcastMessage(String message) throws IOException {
        for (WebSocketSession session : sessions)    // no need to synchronize since using snapshot iterator and session obj is thread safe
            if (session.isOpen())
                session.sendMessage(new TextMessage(message));
    }

    public void addSession(WebSocketSession session) {
        sessions.add(session);
    }

    public void removeSession(WebSocketSession session) {
        sessions.remove(session);
    }
}
