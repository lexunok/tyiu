package com.tyiu.tgbotservice.util;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.ReactiveAuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

@Slf4j
@Component
@RequiredArgsConstructor
public class AuthenticationManager implements ReactiveAuthenticationManager {

    private final JwtCore jwtCore;

    private final UserService userService;

    @Override
    public Mono<Authentication> authenticate(Authentication authentication) {

        String token;
        String id;

        try {
            token = authentication.getCredentials().toString();
            id = jwtCore.getSubject(token);

        } catch (Exception e) {
            token = null;
            id = null;
            log.info(e.toString());
        }
        if (id!=null && jwtCore.isTokenValid(token,id)) {
            Mono<UserDetails> user = userService.findByUsername(id);
            return user.map(u -> new UsernamePasswordAuthenticationToken(u,null,u.getAuthorities()));
        }
        return Mono.empty();
    }
}
