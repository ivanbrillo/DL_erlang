package org.backend;

import org.springframework.stereotype.Component;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

@Component
public class MessageQueues {
    private final BlockingQueue<String> erlangQueue = new LinkedBlockingQueue<>();
    private final BlockingQueue<String> webSocketQueue = new LinkedBlockingQueue<>();

    public void addErlangMessage(String message) throws InterruptedException {
        erlangQueue.put(message);
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

}
