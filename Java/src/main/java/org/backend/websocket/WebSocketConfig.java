package org.backend.websocket;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;
import org.springframework.web.socket.server.support.HttpSessionHandshakeInterceptor;

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
                .setAllowedOriginPatterns("*")
                .addInterceptors(new HttpSessionHandshakeInterceptor()) // Ensure authentication context is carried over
                .withSockJS();
    }
}