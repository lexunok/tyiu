package com.tyiu.notificationservice.config

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity
import org.springframework.security.config.web.server.ServerHttpSecurity
import org.springframework.security.web.server.SecurityWebFilterChain

@Configuration
@EnableWebFluxSecurity
open class SecurityConfig {

    @Bean
    open fun filterChain(http: ServerHttpSecurity): SecurityWebFilterChain {

        http.authorizeExchange {it.anyExchange().permitAll()}
        http.csrf {it.disable()}

        return http.build()
    }
}
