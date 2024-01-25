package com.tyiu.authorizationservice.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import org.springframework.security.core.GrantedAuthority;

import java.util.Collection;

@Getter
@Builder
@AllArgsConstructor
public class IntrospectionPrincipal {

    private String id;
    private String firstName;
    private String lastName;
    private String email;
    private Collection<? extends GrantedAuthority> authorities;

    public static IntrospectionPrincipal build(AuthorizedUser authorizedUser) {
        return IntrospectionPrincipal.builder()
                .id(authorizedUser.getId())
                .firstName(authorizedUser.getFirstName())
                .lastName(authorizedUser.getLastName())
                .email(authorizedUser.getEmail())
                .authorities(authorizedUser.getAuthorities())
                .build();
    }
}