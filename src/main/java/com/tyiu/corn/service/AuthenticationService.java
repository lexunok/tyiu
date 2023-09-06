package com.tyiu.corn.service;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.repository.UserRepository;
import com.tyiu.corn.util.security.CustomUserDetails;
import com.tyiu.corn.util.security.JwtCore;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.userdetails.UserDetails;
import reactor.core.publisher.Mono;

import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AuthenticationService {
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtCore jwtCore;

    public Mono<AuthenticationResponse> login(LoginRequest request) {
        Mono<User> user = userRepository.findFirstByEmail(request.getEmail());
        return user.flatMap(u -> {
            if (passwordEncoder.matches(request.getPassword(), u.getPassword())){
                String jwt = jwtCore.issueToken(u.getEmail(), u.getRoles());
                return Mono.just(AuthenticationResponse.builder()
                        .email(u.getEmail())
                        .token(jwt)
                        .firstName(u.getFirstName())
                        .lastName(u.getLastName())
                        .roles(u.getRoles())
                        .build());
            } else return Mono.empty();
        }).cast(AuthenticationResponse.class).switchIfEmpty(Mono.error(new RuntimeException("User not registered")));
    }


    public Mono<AuthenticationResponse> register(RegisterRequest request){
        Mono<Boolean> isExists = userRepository.existsByEmail(request.getEmail());
        return isExists.flatMap(
                b -> {
                    if (!b) {
                        User user = User.builder()
                                .roles(request.getRoles())
                                .email(request.getEmail())
                                .firstName(request.getFirstName())
                                .lastName(request.getLastName())
                                .password(passwordEncoder.encode(request.getPassword()))
                                .build();
                        try {
                            Mono<User> userFromDB = userRepository.save(user);
                            return userFromDB.flatMap(u -> {
                                String jwt = jwtCore.issueToken(u.getEmail(),u.getRoles());
                                return Mono.just(AuthenticationResponse.builder()
                                        .email(u.getEmail())
                                        .token(jwt)
                                        .firstName(u.getFirstName())
                                        .lastName(u.getLastName())
                                        .roles(u.getRoles())
                                        .build());
                            });
                        }
                        catch (Exception e){
                            userRepository.delete(user);
                            return Mono.empty();
                        }

                    } else return Mono.empty();
                }
        )
                .cast(AuthenticationResponse.class)
                .switchIfEmpty(Mono.error(new RuntimeException("Authorization not success")));
    }
}