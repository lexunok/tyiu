package com.tyiu.tgbotservice.util;

import com.tyiu.tgbotservice.model.Role;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.security.Key;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;
import java.util.Map;

@Component
public class JwtCore {
    @Value("${jwt.secret}")
    String secret;

    public String issueToken(String subject, List<Role> scopes) {
        List<String> roles = scopes.stream().map(Enum::name).toList();
        return issueToken(subject, Map.of("scopes", roles));
    }

    public String issueToken(String subject, Map<String, Object> claims) {

        return Jwts.builder()
                .setClaims(claims)
                .setSubject(subject)
                .setIssuer("hits.tyuiu.ru")
                .setIssuedAt(Date.from(Instant.now()))
                .setExpiration(Date.from(Instant.now().plus(1, ChronoUnit.DAYS)))
                .signWith(getKey(),SignatureAlgorithm.HS256)
                .compact();
    }
    private Key getKey() {
        return Keys.hmacShaKeyFor(secret.getBytes());
    }

    public String getSubject(String token) {
        return getClaims(token).getSubject();
    }

    public Claims getClaims(String token) {

        return Jwts.parserBuilder()
                .setSigningKey(getKey())
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    public boolean isTokenValid(String jwt, String id) {
        String subject = getSubject(jwt);
        return subject.equals(id) && !isTokenExpired(jwt);
    }

    public boolean isTokenExpired(String jwt) {
        return getClaims(jwt).getExpiration().before(Date.from(Instant.now()));
    }
}
