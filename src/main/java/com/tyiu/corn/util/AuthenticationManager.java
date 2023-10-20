package com.tyiu.corn.util;

import io.jsonwebtoken.Claims;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.ReactiveAuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;

@Component
@Slf4j
@RequiredArgsConstructor
public class AuthenticationManager implements ReactiveAuthenticationManager {
    private final JwtCore jwtCore;
    @Override
    public Mono<Authentication> authenticate(Authentication authentication) {
        String token;
        String id;
        try {
            token = authentication.getCredentials().toString();
            id = jwtCore.getSubject(token);
        } catch (Exception e){
            token = null;
            id = null;
            log.info(e.toString());
        }
        if (id!=null && jwtCore.isTokenValid(token,id)){
            Claims claims = jwtCore.getClaims(token);
            @SuppressWarnings("unchecked")
            List<String> roles = claims.get("scopes", List.class);
            List<SimpleGrantedAuthority> authorities = roles.stream().map(SimpleGrantedAuthority::new).toList();
            UsernamePasswordAuthenticationToken authenticationToken =
                    new UsernamePasswordAuthenticationToken(id, null, authorities);
            return Mono.just(authenticationToken);
        }
        return Mono.empty();
    }
}
