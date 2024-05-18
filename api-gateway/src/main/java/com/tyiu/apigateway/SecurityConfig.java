package com.tyiu.apigateway;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.web.server.SecurityWebFilterChain;


@EnableWebFluxSecurity
@Configuration
public class SecurityConfig {
    @Bean
    SecurityWebFilterChain configureFilterChain(ServerHttpSecurity http) {
        return http.authorizeExchange(
                        exchange -> exchange
                                .pathMatchers("/auth/**", "/oauth2/**").permitAll()
                                .anyExchange().authenticated())
                .oauth2Login(Customizer.withDefaults())
                .csrf(ServerHttpSecurity.CsrfSpec::disable)
                .build();
    }
}
