package com.tyiu.scrumservice.config

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.core.convert.converter.Converter
import org.springframework.security.authentication.AbstractAuthenticationToken
import org.springframework.security.config.annotation.method.configuration.EnableReactiveMethodSecurity
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity
import org.springframework.security.config.web.server.ServerHttpSecurity
import org.springframework.security.core.GrantedAuthority
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationConverter
import org.springframework.security.oauth2.server.resource.authentication.JwtGrantedAuthoritiesConverter
import org.springframework.security.web.server.SecurityWebFilterChain
import reactor.core.publisher.Mono


@Configuration
@EnableWebFluxSecurity
@EnableReactiveMethodSecurity
class SecurityConfig {
    @Bean
    fun filterChain(http: ServerHttpSecurity): SecurityWebFilterChain? {
        http
            .authorizeExchange{it.anyExchange().authenticated()}
            .csrf{it.disable()}
            .oauth2ResourceServer { it ->
                it.jwt { its ->
                    its.jwtAuthenticationConverter(getJwtAuthenticationConverter())
            }
            }
        return http.build()
    }

    private fun getJwtAuthenticationConverter(): Converter<Jwt, Mono<AbstractAuthenticationToken>> {
        val jwtAuthenticationConverter = JwtAuthenticationConverter()
        jwtAuthenticationConverter.setJwtGrantedAuthoritiesConverter(getJwtGrantedAuthoritiesConverter())

        return Converter<Jwt, Mono<AbstractAuthenticationToken>> { jwt ->
            jwtAuthenticationConverter.convert(jwt)?.let { Mono.just(it) }
        }
    }

    private fun getJwtGrantedAuthoritiesConverter(): Converter<Jwt?, Collection<GrantedAuthority?>?>? {
        val converter = JwtGrantedAuthoritiesConverter()
        converter.setAuthorityPrefix("ROLE_")
        converter.setAuthoritiesClaimName("roles")
        return converter
    }

}