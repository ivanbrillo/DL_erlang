package org.backend;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class MessageQueues {
    private static final BlockingQueue<String> erlangQueue = new LinkedBlockingQueue<>();
    private static final BlockingQueue<String> webSocketQueue = new LinkedBlockingQueue<>();

    public static void addErlangMessage(String message) throws InterruptedException {
        erlangQueue.put(message);
    }

    public static void addWebSocketMessage(String message) throws InterruptedException {
        webSocketQueue.put(message);
    }

    public static String getErlangMessage() throws InterruptedException {
        return erlangQueue.take();
    }

    public static String getWebSocketMessage() {
        return webSocketQueue.poll();
    }

}
