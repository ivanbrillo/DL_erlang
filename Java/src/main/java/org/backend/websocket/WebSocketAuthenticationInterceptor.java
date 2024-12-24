package org.backend.websocket;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.JwtException;
import org.springframework.web.socket.server.HandshakeInterceptor;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.socket.WebSocketHandler;

import java.util.Map;
import java.net.URI;

public class WebSocketAuthenticationInterceptor implements HandshakeInterceptor {

    private final String jwtSecret;

    public WebSocketAuthenticationInterceptor(String jwtSecret) {
        this.jwtSecret = jwtSecret;
    }

    @Override
    public boolean beforeHandshake(ServerHttpRequest request, ServerHttpResponse response,
                                   WebSocketHandler wsHandler, Map<String, Object> attributes) throws Exception {
        String token = extractToken(request.getURI());

        if (token == null || !isValidToken(token)) {
            return false;
        }

        // Store user info in attributes if needed
        String username = extractUsernameFromToken(token);
        attributes.put("username", username);

        return true;
    }

    @Override
    public void afterHandshake(ServerHttpRequest request, ServerHttpResponse response,
                               WebSocketHandler wsHandler, Exception ex) {
        // You can perform actions after the handshake if needed
    }

    private String extractToken(URI uri) {
        String query = uri.getQuery();
        if (query != null && query.startsWith("token=")) {
            return query.substring(6);
        }
        return null;
    }

    private boolean isValidToken(String token) {
        try {
            Jwts.parser()
                    .setSigningKey(jwtSecret)
                    .parseClaimsJws(token);
            return true;
        } catch (JwtException e) {
            return false;
        }
    }

    private String extractUsernameFromToken(String token) {
        return Jwts.parser()
                .setSigningKey(jwtSecret)
                .parseClaimsJws(token)
                .getBody()
                .getSubject();
    }
}