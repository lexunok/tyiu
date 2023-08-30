package com.tyiu.corn.util.security;

import io.jsonwebtoken.Jwt;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.ReactiveAuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
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
            Mono<UserDetails> userDetails = userService.findByUsername(email);
            userDetails.flatMap(u -> {
                UsernamePasswordAuthenticationToken authenticationToken =
                        new UsernamePasswordAuthenticationToken(u, null, u.getAuthorities());
                return Mono.just(authentication);
            });
        }
        return Mono.empty();
    }
}
