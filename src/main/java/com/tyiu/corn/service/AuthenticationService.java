package com.tyiu.corn.service;

import com.tyiu.corn.model.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.LoginRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.repository.UserRepository;
import com.tyiu.corn.util.security.CustomUserDetails;
import com.tyiu.corn.util.security.JwtCore;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
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

    public String login(LoginRequest request){
        Authentication authentication = null;
        try {
            authentication = authenticationManager
                    .authenticate(new UsernamePasswordAuthenticationToken(request.getUsername(),request.getPassword()));
        }
        catch (Exception e){
            log.error(e.toString());
        }
        if (authentication != null) {
            String jwt = jwtCore.generateToken(authentication);
            SecurityContextHolder.getContext().setAuthentication(authentication);
            return jwt;
        }
        return "Ошибка";
    }
    public void register(RegisterRequest request){
        User user = User.builder()
                .roles(List.of(Role.ADMIN))
                .email(request.getEmail())
                .firstName(request.getFirstName())
                .lastName(request.getLastName())
                .username(request.getUsername())
                .password(passwordEncoder.encode(request.getPassword()))
                .build();
        userRepository.save(user);
    }
}
