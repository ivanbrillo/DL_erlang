package org.backend.websocket;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;

import static org.springframework.security.config.Customizer.withDefaults;

@Configuration
@EnableWebSecurity
public class SecurityConfig {

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers("/login").permitAll() // Allow access to login and static resources
                        .anyRequest().authenticated() // Require authentication for every other request
                )
                .formLogin(withDefaults())// Ensure the login page is the first thing they see
                .httpBasic(withDefaults()); // Keep basic authentication if needed (but formLogin will take precedence)

        return http.build();
    }




}
