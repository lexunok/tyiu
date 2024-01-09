package com.tyiu.ideas.service;

import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.requests.LoginRequest;
import com.tyiu.ideas.model.requests.RegisterRequest;
import com.tyiu.ideas.model.responses.AuthenticationResponse;
import com.tyiu.ideas.util.JwtCore;
import lombok.RequiredArgsConstructor;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import reactor.core.publisher.Mono;

import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
public class AuthenticationService {

    private final R2dbcEntityTemplate template;
    private final PasswordEncoder passwordEncoder;
    private final JwtCore jwtCore;

    public Mono<AuthenticationResponse> login(LoginRequest request) {
        Mono<User> user = template
                .selectOne(query(where("email").is(request.getEmail())),User.class)
                .switchIfEmpty(Mono.error(new NotFoundException("Авторизация не удалась!")));
        return user.flatMap(u -> {
            if (passwordEncoder.matches(request.getPassword(), u.getPassword())){
                String jwt = jwtCore.issueToken(String.valueOf(u.getId()), u.getRoles());
                return Mono.just(AuthenticationResponse.builder()
                        .id(u.getId())
                        .email(u.getEmail())
                        .token(jwt)
                        .firstName(u.getFirstName())
                        .lastName(u.getLastName())
                        .roles(u.getRoles())
                        .createdAt(u.getCreatedAt())
                        .build());
            } else return Mono.error(new NotFoundException("Авторизация не удалась!"));
        });
    }


    public Mono<AuthenticationResponse> register(RegisterRequest request){
        Mono<Boolean> isExists = template
                .exists(query(where("email").is(request.getEmail())), User.class);
        return isExists.flatMap(
                b -> {
                    if (Boolean.FALSE.equals(b)) {
                        User user = User.builder()
                                .roles(request.getRoles())
                                .email(request.getEmail())
                                .firstName(request.getFirstName())
                                .lastName(request.getLastName())
                                .password(passwordEncoder.encode(request.getPassword()))
                                .createdAt(LocalDateTime.now())
                                .build();
                        try {
                            Mono<User> userFromDB = template.insert(user);
                            return userFromDB.flatMap(u -> {
                                String jwt = jwtCore.issueToken(String.valueOf(u.getId()),u.getRoles());
                                return Mono.just(AuthenticationResponse.builder()
                                        .id(u.getId())
                                        .email(u.getEmail())
                                        .token(jwt)
                                        .firstName(u.getFirstName())
                                        .lastName(u.getLastName())
                                        .roles(u.getRoles())
                                        .createdAt(u.getCreatedAt())
                                        .build());
                            });
                        }
                        catch (Exception e){
                            template.delete(user).subscribe();
                            return Mono.error(new NotFoundException("Регистрация не удалась!"));
                        }

                    } else return Mono.error(new NotFoundException("Регистрация не удалась!"));
                }
        );
    }
}