package com.tyiu.corn.service;

import com.tyiu.corn.exception.UserExistsException;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.repository.UserRepository;
import com.tyiu.corn.util.security.CustomUserDetails;
import com.tyiu.corn.util.security.JwtCore;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.annotation.CurrentSecurityContext;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.refEq;
import static org.mockito.Mockito.*;

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
    void registerSuccess() {
        // Given
        RegisterRequest request = new RegisterRequest(
                "edmail","lastnasme","firsdstname","psdassword",List.of(Role.ADMIN,Role.EXPERT));
        UsernamePasswordAuthenticationToken token = new UsernamePasswordAuthenticationToken(request.getEmail(),request.getPassword());
        Authentication authentication = authenticationManager.authenticate(token);
        when(userRepository.existsByEmail(request.getEmail())).thenReturn(false);
        when(authenticationManager.authenticate(token)).thenReturn(authentication);
        when(jwtCore.generateToken(authentication)).thenReturn("sdfsdfsdfsdfsd");
        // When
        AuthenticationResponse response = underTest.register(request);
        // Then
        ArgumentCaptor<User> captor = ArgumentCaptor.forClass(User.class);
        verify(userRepository).save(captor.capture());
        verify(passwordEncoder).encode(request.getPassword());
        User user = captor.getValue();
        assertEquals(user.getEmail(),request.getEmail());
        assertNotNull(response.getToken());
        assertDoesNotThrow(() -> underTest.register(request));
    }
    @Test
    void registerWhenUserExists() {
        // Given
        RegisterRequest request = new RegisterRequest(
                "edmail","lastnasme","firsdstname","psdassword",List.of(Role.ADMIN,Role.EXPERT));
        when(userRepository.existsByEmail(request.getEmail())).thenReturn(true);
        // When
        // Then
        verify(userRepository, never()).save(any());
        assertThrows(UserExistsException.class, ()->underTest.register(request));
    }
}