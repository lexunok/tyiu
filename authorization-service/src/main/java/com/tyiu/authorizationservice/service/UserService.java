package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.dto.AuthorizedUser;
import com.tyiu.authorizationservice.model.entities.User;
import com.tyiu.authorizationservice.model.enums.AuthProvider;
import org.springframework.security.oauth2.core.user.OAuth2User;

public interface UserService {

    User save(OAuth2User userDto, AuthProvider provider);

    AuthorizedUser saveAndMap(OAuth2User userDto, AuthProvider provider);

}