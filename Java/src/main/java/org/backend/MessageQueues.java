package org.backend;

import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

@Component
public class MessageQueues {
    private final BlockingQueue<String> erlangQueue = new LinkedBlockingQueue<>();
    private final BlockingQueue<String> webSocketQueue = new LinkedBlockingQueue<>();

    private final BlockingQueue<String> cacheSession = new LinkedBlockingQueue<>();
    private final List<String> cacheMsgCodes = List.of("{initialized_nodes", "{train_epoch_completed",
            "{training_total_completed", "{node_up", "{node_down", "{db_ack", "{new_train");

    public synchronized void addErlangMessage(String message) throws InterruptedException {
        erlangQueue.put(message);

        if (cacheMsgCodes.stream().anyMatch(message::startsWith))
            cacheSession.put(message);

    }

    public void addWebSocketMessage(String message) throws InterruptedException {
        webSocketQueue.put(message);
    }

    //TODO synch?
    public String getErlangMessage() throws InterruptedException {
        return erlangQueue.take();
    }

    public String getWebSocketMessage() {
        return webSocketQueue.poll();
    }

    public synchronized void restoreSession() {
        erlangQueue.clear();
        erlangQueue.addAll(cacheSession);
    }

    public synchronized void clearCache() {
        cacheSession.clear();
    }


}
