package org.backend;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class MessageQueues {
    public static final BlockingQueue<String> erlangQueue = new LinkedBlockingQueue<>();
    public static final BlockingQueue<String> webSocketQueue = new LinkedBlockingQueue<>();
}
