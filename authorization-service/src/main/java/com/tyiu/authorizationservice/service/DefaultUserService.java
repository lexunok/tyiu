package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.dto.AuthorizedUser;
import com.tyiu.authorizationservice.model.entities.User;
import com.tyiu.authorizationservice.model.enums.AuthErrorCode;
import com.tyiu.authorizationservice.model.enums.Role;
import com.tyiu.authorizationservice.model.exceptions.AuthException;
import com.tyiu.authorizationservice.model.mappers.AuthorizedUserMapper;
import com.tyiu.authorizationservice.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;
import com.tyiu.authorizationservice.model.enums.AuthProvider;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class DefaultUserService implements UserService {

    private final UserRepository userRepository;

    @Override
    @Transactional
    public User save(OAuth2User userDto, AuthProvider provider) {
        return switch (provider) {
            case GITHUB -> saveUserFromGithub(userDto);
            case GOOGLE -> saveUserFromGoogle(userDto);
            case YANDEX -> saveUserFromYandex(userDto);
        };
    }

    @Override
    public AuthorizedUser saveAndMap(OAuth2User userDto, AuthProvider provider) {
        User entity = save(userDto, provider);
        return AuthorizedUserMapper.map(entity, provider);
    }

    private User saveUserFromGithub(OAuth2User userDto) {
        User user = getEntityByEmail(userDto.getAttribute("email"));

        if (userDto.getAttribute("name") != null) {
            String[] splitted = ((String) userDto.getAttribute("name")).split(" ");
            user.setFirstName(splitted[0]);
            if (splitted.length > 1) {
                user.setLastName(splitted[1]);
            }
        } else {
            user.setFirstName(userDto.getAttribute("login"));
            user.setLastName(userDto.getAttribute("login"));
        }

        return userRepository.save(user);
    }

    private User saveUserFromGoogle(OAuth2User userDto) {
        User user = getEntityByEmail(userDto.getAttribute("email"));

        if (userDto.getAttribute("given_name") != null) {
            user.setFirstName(userDto.getAttribute("given_name"));
        }

        if (userDto.getAttribute("family_name") != null) {
            user.setLastName(userDto.getAttribute("family_name"));
        }

        return userRepository.save(user);
    }

    private User saveUserFromYandex(OAuth2User userDto) {
        User user = getEntityByEmail(userDto.getAttribute("default_email"));

        if (userDto.getAttribute("first_name") != null) {
            user.setFirstName(userDto.getAttribute("first_name"));
        }

        if (userDto.getAttribute("last_name") != null) {
            user.setLastName(userDto.getAttribute("last_name"));
        }

        return userRepository.save(user);
    }

    private User getEntityByEmail(String email) {
        if (email == null) {
            throw new AuthException(AuthErrorCode.EMAIL_IS_EMPTY);
        }
        User user = userRepository.findByEmail(email);
        if (user == null) {
            user = new User();
            user.setEmail(email);
            user.setRoles(List.of(Role.INITIATOR));
        }
        return user;
    }
}