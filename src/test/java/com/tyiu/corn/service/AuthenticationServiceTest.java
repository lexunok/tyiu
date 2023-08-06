package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.repository.UserRepository;
import com.tyiu.corn.util.security.CustomUserDetails;
import com.tyiu.corn.util.security.JwtCore;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.annotation.CurrentSecurityContext;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.refEq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AuthenticationServiceTest {
    private AuthenticationService underTest;
    @Mock
    private UserRepository userRepository;
    @Mock
    private PasswordEncoder passwordEncoder;
    @Mock
    private AuthenticationManager authenticationManager;
    @Mock
    private JwtCore jwtCore;

    @BeforeEach
    void setUp() {
        underTest = new AuthenticationService(userRepository,passwordEncoder,authenticationManager,jwtCore);
    }


    @Test
    void login() {
        // Given
        LoginRequest request = new LoginRequest("email","password");
        // When
        underTest.login(request);
        // Then
        verify(authenticationManager)
                .authenticate(new UsernamePasswordAuthenticationToken(request.getEmail(),request.getPassword()));
    }

    @Test
    void register() {
        // Given
        RegisterRequest request = new RegisterRequest(
                "email","lastname","firstname","password",List.of(Role.ADMIN));
        User user = User.builder()
                .roles(List.of(Role.ADMIN))
                .email(request.getEmail())
                .firstName(request.getFirstName())
                .lastName(request.getLastName())
                .password(passwordEncoder.encode(request.getPassword()))
                .build();
        // When
        underTest.register(request);
        // Then
        verify(userRepository).save(refEq(user));
        verify(authenticationManager)
                .authenticate(new UsernamePasswordAuthenticationToken(request.getEmail(),request.getPassword()));
    }
}