package com.tyiu.scrumservice.config

import org.springframework.beans.factory.annotation.Value
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
    @Value("\${spring.security.oauth2.resourceserver.jwt.issuer-uri}")
    val issuer: String? = null
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
        return NimbusJwtDecoder.withIssuerLocation(issuer).build()
    }
}