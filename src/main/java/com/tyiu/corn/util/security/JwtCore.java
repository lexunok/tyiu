package com.tyiu.corn.util.security;

import com.tyiu.corn.util.security.CustomUserDetails;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
public class JwtCore {
    @Value("${jwt.secret}")
    private String secret;
    @Value("${jwt.expiration}")
    private Long expiration;

    public String generateToken(Authentication authentication){
        CustomUserDetails userDetails = (CustomUserDetails) authentication.getPrincipal();
        Date date = new Date();
        return Jwts.builder()
                    .setSubject(userDetails.getUsername())
                    .setIssuedAt(date)
                    .setExpiration(new Date(date.getTime() + expiration))
                    .signWith(SignatureAlgorithm.HS256,secret)
                .compact();
    }
    public String getNameFromJwt(String token){
        return Jwts.parser().setSigningKey(secret).parseClaimsJws(token).getBody().getSubject();
    }
}
