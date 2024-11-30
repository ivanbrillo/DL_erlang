package org.backend;

import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;

@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {

    private final ErlangWebSocketHandler erlangWebSocketHandler;

    public WebSocketConfig(ErlangWebSocketHandler erlangWebSocketHandler) {
        this.erlangWebSocketHandler = erlangWebSocketHandler;
        System.out.println("[WebSocket] Setting up websocket handler");
    }

    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        registry.addHandler(erlangWebSocketHandler, "/erlang-socket")
                .setAllowedOrigins("*");  // Adjust origin restrictions as needed
    }
}