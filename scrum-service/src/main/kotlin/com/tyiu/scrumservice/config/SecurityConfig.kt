package com.tyiu.scrumservice.config

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.security.config.Customizer
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity
import org.springframework.security.config.web.server.ServerHttpSecurity
import org.springframework.security.oauth2.jwt.JwtDecoder
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder
import org.springframework.security.web.server.SecurityWebFilterChain


@Configuration
@EnableWebFluxSecurity
class SecurityConfig {
    @Bean
    fun filterChain(http: ServerHttpSecurity): SecurityWebFilterChain? {
        http
            .authorizeExchange{it.anyExchange().authenticated()}
            .csrf{it.disable()}
            .oauth2ResourceServer { it.jwt(Customizer.withDefaults()) }
        return http.build()
    }
    @Bean
    fun jwtDecoder(): JwtDecoder {
        return NimbusJwtDecoder.withIssuerLocation("http://127.0.0.1:7777").build()
    }
}