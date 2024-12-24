package org.backend.websocket;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;
import org.springframework.web.socket.server.HandshakeInterceptor;


@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {

    @Value("${jwt.secret}")
    private String jwtSecret;

    private final ErlangWebSocketHandler erlangWebSocketHandler;

    public WebSocketConfig(ErlangWebSocketHandler erlangWebSocketHandler) {
        this.erlangWebSocketHandler = erlangWebSocketHandler;
        System.out.println("[WebSocket] Setting up websocket handler");
    }

    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        registry.addHandler(erlangWebSocketHandler, "/erlang-socket")
                .addInterceptors(new WebSocketAuthenticationInterceptor(jwtSecret))
                .setAllowedOrigins("*");
    }
}