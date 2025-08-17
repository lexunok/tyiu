package com.tyiu.ideas.config;


import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.method.configuration.EnableReactiveMethodSecurity;
import org.springframework.security.config.annotation.rsocket.RSocketSecurity;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.ReactiveJwtDecoder;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationConverter;
import org.springframework.security.oauth2.server.resource.authentication.JwtGrantedAuthoritiesConverter;
import org.springframework.security.oauth2.server.resource.authentication.JwtReactiveAuthenticationManager;
import org.springframework.security.rsocket.core.PayloadSocketAcceptorInterceptor;
import org.springframework.security.web.server.SecurityWebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Collection;


@Configuration
@EnableWebFluxSecurity
@EnableReactiveMethodSecurity
@RequiredArgsConstructor
public class SecurityConfig {
    private final ReactiveJwtDecoder reactiveJwtDecoder;
    @Bean
    public SecurityWebFilterChain securityWebFilterChain(ServerHttpSecurity http) {
        http
                .csrf(ServerHttpSecurity.CsrfSpec::disable)
                .httpBasic(ServerHttpSecurity.HttpBasicSpec::disable)
                .formLogin(ServerHttpSecurity.FormLoginSpec::disable)
                .cors(Customizer.withDefaults())
                .authorizeExchange(authorize -> authorize
                        .pathMatchers("/api/v1/ideas-service/profile").permitAll()
                        .pathMatchers("/v3/api-docs/**").permitAll()
                        .pathMatchers("/swagger-ui/**").permitAll()
                        .pathMatchers("/swagger-ui.html").permitAll()
                        .pathMatchers("/webjars/swagger-ui/**").permitAll()
                        .pathMatchers("/swagger-resources/**").permitAll()
                        .anyExchange().authenticated())
                .oauth2ResourceServer(resource -> resource
                        .jwt(jwt ->
                                jwt.jwtAuthenticationConverter(getJwtAuthenticationConverter())));
        return http.build();
    }

    @Bean
    public PayloadSocketAcceptorInterceptor rsocketInterceptor(RSocketSecurity rsocket) {
        JwtReactiveAuthenticationManager authenticationManager =
                new JwtReactiveAuthenticationManager(reactiveJwtDecoder);

        authenticationManager.setJwtAuthenticationConverter(getJwtAuthenticationConverter()::convert);
        rsocket
                .authenticationManager(authenticationManager)
                .jwt(jwt -> jwt.authenticationManager(authenticationManager))
                .authorizePayload(authorize -> authorize
                        .setup().permitAll() // Разрешаем установку соединения
                        .anyRequest().authenticated() // Требуем аутентификацию для запросов
                        .anyExchange().permitAll()
                );
        return rsocket.build();
    }

    private Converter<Jwt, Mono<AbstractAuthenticationToken>> getJwtAuthenticationConverter(){
        JwtAuthenticationConverter jwtAuthenticationConverter = new JwtAuthenticationConverter();
        jwtAuthenticationConverter.setJwtGrantedAuthoritiesConverter(getJwtGrantedAuthoritiesConverter());

        return jwt -> {
            AbstractAuthenticationToken authenticationToken = jwtAuthenticationConverter.convert(jwt);
            return Mono.justOrEmpty(authenticationToken);
        };
    }


    private Converter<Jwt, Collection<GrantedAuthority>> getJwtGrantedAuthoritiesConverter(){
        JwtGrantedAuthoritiesConverter converter = new JwtGrantedAuthoritiesConverter();
        converter.setAuthorityPrefix("ROLE_");
        converter.setAuthoritiesClaimName("roles");
        return converter;
    }


}