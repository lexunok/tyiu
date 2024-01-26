package com.tyiu.authorizationservice.config.security;

import com.tyiu.authorizationservice.service.CustomOAuth2UserService;
import com.tyiu.authorizationservice.service.CustomUserDetailsService;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;

import static org.springframework.security.config.Customizer.withDefaults;

@EnableWebSecurity
@EnableMethodSecurity
@RequiredArgsConstructor
@Configuration(proxyBeanMethods = false)
public class SecurityConfig {

    private final CustomOAuth2UserService customOAuth2UserService;
    private final CustomUserDetailsService userDetailService;

    private final PasswordEncoder passwordEncoder;

    @Bean
    public SecurityFilterChain defaultSecurityFilterChain(HttpSecurity http) throws Exception {
        SocialConfigurer socialConfigurer = new SocialConfigurer()
                .oAuth2UserService(customOAuth2UserService);
        http.apply(socialConfigurer);

        http.getSharedObject(AuthenticationManagerBuilder.class)
                .userDetailsService(userDetailService)
                .passwordEncoder(passwordEncoder);

        http.authorizeHttpRequests(authorize ->
                authorize.anyRequest().authenticated()
        );
        return http.formLogin(withDefaults()).build();
    }
}
