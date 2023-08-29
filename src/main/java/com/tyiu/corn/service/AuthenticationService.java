package com.tyiu.corn.service;

import com.tyiu.corn.exception.AuthorizationNotSuccessException;
import com.tyiu.corn.exception.UserExistsException;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.repository.UserRepository;
import com.tyiu.corn.util.security.CustomUserDetails;
import com.tyiu.corn.util.security.JwtCore;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import reactor.core.publisher.Mono;

import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class AuthenticationService {
//    private final UserRepository userRepository;
//    private final PasswordEncoder passwordEncoder;
//    private final AuthenticationManager authenticationManager;
//    private final JwtCore jwtCore;
//
//    public Mono<AuthenticationResponse> login(LoginRequest request){
//        return Mono.fromCallable(() -> authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(request.getEmail(),request.getPassword())))
//            .flatMap(authentication -> {
//                if (authentication != null){
//                    CustomUserDetails userDetails = (CustomUserDetails) authentication.getPrincipal();
//                    return userRepository.findByEmail(userDetails.getUsername())
//                        .switchIfEmpty(Mono.error(new UsernameNotFoundException("Пользователь не зарегистрирован")))
//                        .flatMap(user -> {
//                            String jwt = jwtCore.issueToken(user.getEmail(),user.getRoles());
//                            return Mono.just(AuthenticationResponse.builder()
//                                .email(user.getEmail())
//                                .token(jwt)
//                                .firstName(user.getFirstName())
//                                .lastName(user.getLastName())
//                                .roles(user.getRoles())
//                                .build());
//                        });
//                } else {
//                    return Mono.error(new AuthorizationNotSuccessException("Авторизация не удалась"));
//                }
//            });
//    }
//
//    public Mono<AuthenticationResponse> register(RegisterRequest request){
//        return userRepository.existsByEmail(request.getEmail())
//            .flatMap(exists -> {
//                if (!exists){
//                    User user = User.builder()
//                        .roles(request.getRoles())
//                        .email(request.getEmail())
//                        .firstName(request.getFirstName())
//                        .lastName(request.getLastName())
//                        .password(passwordEncoder.encode(request.getPassword()))
//                        .build();
//                    return userRepository.save(user)
//                        .flatMap(savedUser -> {
//                            return Mono.fromCallable(() -> authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(request.getEmail(),request.getPassword())))
//                                .flatMap(authentication -> {
//                                    CustomUserDetails userDetails = (CustomUserDetails) authentication.getPrincipal();
//                                    String jwt = jwtCore.issueToken(userDetails.getUsername(),savedUser.getRoles());
//                                    return Mono.just(AuthenticationResponse.builder()
//                                        .email(savedUser.getEmail())
//                                        .token(jwt)
//                                        .firstName(savedUser.getFirstName())
//                                        .lastName(savedUser.getLastName())
//                                        .roles(savedUser.getRoles())
//                                        .build());
//                                });
//                        })
//                        .onErrorResume(e -> {
//                            userRepository.delete(user).subscribe();
//                            return Mono.error(new AuthorizationNotSuccessException("Авторизация не удалась"));
//                        });
//                } else {
//                    return Mono.error(new UserExistsException("Пользователь c такой почтой существует"));
//                }
//            });
//    }
}