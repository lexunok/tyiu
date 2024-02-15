package com.tyiu.scrumservice.config

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.security.config.Customizer
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity
import org.springframework.security.config.web.server.ServerHttpSecurity
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
}