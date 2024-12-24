package org.backend.controller;

import org.backend.dto.AuthRequest;
import org.backend.dto.AuthResponse;
import org.backend.security.JwtTokenUtil;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class AuthController {

    private final JwtTokenUtil jwtTokenUtil;

    public AuthController(JwtTokenUtil jwtTokenUtil) {
        this.jwtTokenUtil = jwtTokenUtil;
    }

    @PostMapping("/authenticate")
    public ResponseEntity<?> authenticate(@RequestBody AuthRequest authRequest) {
        if ("user@example.com".equals(authRequest.getEmail()) && "password".equals(authRequest.getPassword())) {
            String token = jwtTokenUtil.generateToken(authRequest.getEmail());
            return ResponseEntity.ok(new AuthResponse(token));
        } else {
            return ResponseEntity.status(401).body("Credenziali non valide");
        }
    }
}
