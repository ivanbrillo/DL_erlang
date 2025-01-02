package org.backend.websocket;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;
import org.springframework.web.socket.server.support.HttpSessionHandshakeInterceptor;

@Configuration
@EnableWebSocket
@Slf4j
public class WebSocketConfig implements WebSocketConfigurer {

    private final ErlangWebSocketHandler erlangWebSocketHandler;

    public WebSocketConfig(ErlangWebSocketHandler erlangWebSocketHandler) {
        this.erlangWebSocketHandler = erlangWebSocketHandler;
    }

    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        registry.addHandler(erlangWebSocketHandler, "/erlang-socket")
                .setAllowedOriginPatterns("*")
                .addInterceptors(new HttpSessionHandshakeInterceptor()) // Ensure authentication context is carried over
                .withSockJS();
        log.info("[WebSocket] Websocket handler setup correctly");
    }
}