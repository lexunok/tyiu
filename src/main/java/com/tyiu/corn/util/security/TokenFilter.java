package com.tyiu.corn.util.security;

import io.jsonwebtoken.ExpiredJwtException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
@RequiredArgsConstructor
@Slf4j
public class TokenFilter extends OncePerRequestFilter {

    private final UserService userService;
    private final JwtCore jwtCore;
    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        try{
            String email = null;
            String jwt = null;
            String header = request.getHeader("Authorization");
            if (header != null && header.startsWith("Bearer ")){
                jwt = header.substring(7);
            }
            if (jwt!=null){
                try {
                    email = jwtCore.getNameFromJwt(jwt);
                } catch (ExpiredJwtException e) {
                    log.error(e.toString());
                }
                if (email != null && SecurityContextHolder.getContext().getAuthentication() == null){
                    UserDetails userDetails = userService.loadUserByUsername(email);
                    SecurityContextHolder.getContext()
                            .setAuthentication(new UsernamePasswordAuthenticationToken(userDetails, null));
                }
            }
        }
        catch (Exception e) {
            log.error(e.toString());
        }
        filterChain.doFilter(request,response);
    }
}
