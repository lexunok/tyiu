package com.tyiu.corn.util.security;

import com.tyiu.corn.model.enums.Role;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwt;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.security.authentication.ReactiveAuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;

@Component
@Slf4j
@RequiredArgsConstructor
public class AuthenticationManager implements ReactiveAuthenticationManager {
    private final JwtCore jwtCore;
    private final UserService userService;
    @Override
    public Mono<Authentication> authenticate(Authentication authentication) {
        String token;
        String email;
        try {
            token = authentication.getCredentials().toString();
            email = jwtCore.getSubject(token);
        } catch (Exception e){
            token = null;
            email = null;
            log.info(e.toString());
        }
        if (email!=null && jwtCore.isTokenValid(token,email)){
            Claims claims = jwtCore.getClaims(token);
            @SuppressWarnings("unchecked")
            List<String> roles = claims.get("scopes", List.class);
            List<SimpleGrantedAuthority> authorities = roles.stream().map(SimpleGrantedAuthority::new).toList();
            UsernamePasswordAuthenticationToken authenticationToken =
                    new UsernamePasswordAuthenticationToken(email, null, authorities);
            log.info(authenticationToken.toString());
            return Mono.just(authenticationToken);
        }
        return Mono.empty();
    }
}