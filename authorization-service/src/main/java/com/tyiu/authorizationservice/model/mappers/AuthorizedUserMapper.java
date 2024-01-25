package com.tyiu.authorizationservice.model.mappers;

import com.tyiu.authorizationservice.model.dto.AuthorizedUser;
import com.tyiu.authorizationservice.model.entities.User;
import com.tyiu.authorizationservice.model.enums.AuthProvider;
import lombok.experimental.UtilityClass;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.List;
import java.util.stream.Collectors;

@UtilityClass
public class AuthorizedUserMapper {

    public AuthorizedUser map(User entity, AuthProvider provider) {
        List<GrantedAuthority> authorities = getUserAuthorities(entity);
        return AuthorizedUser.builder(entity.getEmail(), entity.getPassword(), authorities)
                .id(entity.getId())
                .firstName(entity.getFirstName())
                .secondName(entity.getLastName())
                .build();
    }

    public List<GrantedAuthority> getUserAuthorities(User entity) {
        return entity.getRoles().stream().map(authority ->
                        new SimpleGrantedAuthority(String.valueOf(authority)))
                .collect(Collectors.toList());
    }
}