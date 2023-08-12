package com.tyiu.corn.service;

import com.tyiu.corn.exception.AuthorizationNotSuccessException;
import com.tyiu.corn.exception.UserExistsException;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.repository.UserRepository;
import com.tyiu.corn.util.security.JwtCore;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class AuthenticationService {
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final AuthenticationManager authenticationManager;
    private final JwtCore jwtCore;

    public AuthenticationResponse login(LoginRequest request){
        Authentication authentication;
        User user;
        try {
            authentication = authenticationManager
                    .authenticate(new UsernamePasswordAuthenticationToken(request.getEmail(),request.getPassword()));
        }
        catch (Exception e){
            throw new AuthorizationNotSuccessException("Авторизация не удалась");
        }
        if (authentication != null) {
            String jwt = jwtCore.generateToken(authentication);
            SecurityContextHolder.getContext().setAuthentication(authentication);
            user = userRepository.findByEmail(request.getEmail())
                    .orElseThrow(() -> new UsernameNotFoundException("Пользователь не найден"));
            return AuthenticationResponse.builder()
                    .email(user.getEmail())
                    .token(jwt)
                    .firstName(user.getFirstName())
                    .lastName(user.getLastName())
                    .roles(user.getRoles())
                    .build();
        }
        else return null;
    }
    public AuthenticationResponse register(RegisterRequest request){
        if (!userRepository.existsByEmail(request.getEmail())){
            User user = User.builder()
                    .roles(request.getRoles())
                    .email(request.getEmail())
                    .firstName(request.getFirstName())
                    .lastName(request.getLastName())
                    .password(passwordEncoder.encode(request.getPassword()))
                    .build();
            try {
                userRepository.save(user);
                Authentication authentication = authenticationManager
                        .authenticate(new UsernamePasswordAuthenticationToken(request.getEmail(),request.getPassword()));
                String jwt = jwtCore.generateToken(authentication);
                SecurityContextHolder.getContext().setAuthentication(authentication);
                return AuthenticationResponse.builder()
                        .email(user.getEmail())
                        .token(jwt)
                        .firstName(user.getFirstName())
                        .lastName(user.getLastName())
                        .roles(user.getRoles())
                        .build();
            }
            catch (Exception e){
                userRepository.delete(user);
                throw new AuthorizationNotSuccessException("Авторизация не удалась");
            }
        }
        else throw new UserExistsException("Пользователь с такой почтой существует");
    }
}
