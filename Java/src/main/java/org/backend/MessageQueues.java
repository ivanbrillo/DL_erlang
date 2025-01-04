package org.backend;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

@Component
public class MessageQueues {
    private final BlockingQueue<String> erlangQueue = new LinkedBlockingQueue<>();
    private final BlockingQueue<String> webSocketQueue = new LinkedBlockingQueue<>();

    private final BlockingQueue<String> cacheSession = new LinkedBlockingQueue<>();
    private final List<String> cacheMsgCodes = List.of("initialized_nodes", "train",
            "node_up", "node_down", "db_ack", "stopped");

    public synchronized void addErlangMessage(String message) throws InterruptedException {
        erlangQueue.put(message);

        if (cacheMsgCodes.stream().anyMatch(code -> message.startsWith("{" + code)))
            cacheSession.put(message);

        if(message.startsWith("{new_train")) {   // clear all old train messages since they will not be needed anymore
            cacheSession.removeIf(msg -> msg.startsWith("{train"));
            cacheSession.put(message);
        }
    }

    public void addWebSocketMessage(String message) throws InterruptedException {
        webSocketQueue.put(message);
    }

    public String getErlangMessage() throws InterruptedException {
        return erlangQueue.take();
    }

    public String getWebSocketMessage() {
        return webSocketQueue.poll();
    }

    public synchronized void restoreSession(WebSocketSession session) throws RuntimeException, InterruptedException {
        long startTime = System.currentTimeMillis();
        long timeoutMillis = 500;

        // Wait for erlangQueue to empty otherwise could have some message duplication on the new client
        // no new msgs can be added when performing restoreSession
        while (!erlangQueue.isEmpty()) {
            if (System.currentTimeMillis() - startTime > timeoutMillis)
                throw new RuntimeException("Timeout waiting for erlang queue to empty after " + timeoutMillis + " ms");

            Thread.sleep(5);
        }

        cacheSession.forEach(msg -> {
            try {
                session.sendMessage(new TextMessage(msg));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }

    public synchronized void clearCache() {   // called during Stop command
        cacheSession.clear();
        try {
            addErlangMessage("{stopped}");   // clear the UI
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException(e);
        }
    }


}
