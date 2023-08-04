package com.tyiu.corn.util.security;

import io.jsonwebtoken.ExpiredJwtException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
@RequiredArgsConstructor
public class TokenFilter extends OncePerRequestFilter {

    private final UserService userService;
    private final JwtCore jwtCore;
    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        String jwt = null;
        String username = null;
        UserDetails userDetails = null;
        UsernamePasswordAuthenticationToken auth = null;
        try{
            String header = request.getHeader("Authorization");
            if (header != null && header.startsWith("Bearer ")){
                jwt = header.substring(7);
            }
            if (jwt!=null){
                try {
                    username = jwtCore.getNameFromJwt(jwt);
                } catch (ExpiredJwtException e) {
                    logger.error(e);
                }
                if (username != null && SecurityContextHolder.getContext().getAuthentication() == null){
                    userDetails = userService.loadUserByUsername(username);
                    auth = new UsernamePasswordAuthenticationToken(userDetails, null);
                    SecurityContextHolder.getContext().setAuthentication(auth);
                }
            }
        }
        catch (Exception e) {
            logger.error(e);
        }
        filterChain.doFilter(request,response);
    }
}
