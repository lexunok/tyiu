package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.dto.AuthorizedUser;
import com.tyiu.authorizationservice.model.entities.UserEntity;
import com.tyiu.authorizationservice.model.enums.AuthErrorCode;
import com.tyiu.authorizationservice.model.exceptions.AuthException;
import com.tyiu.authorizationservice.model.mappers.AuthorizedUserMapper;
import com.tyiu.authorizationservice.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;
import com.tyiu.authorizationservice.model.enums.AuthProvider;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

@Service
@RequiredArgsConstructor
public class DefaultUserService implements UserService {

    @Value("${yandex-avatar-url}")
    private String yandexAvatarUrl;
    private final UserRepository userRepository;

    @Override
    public UserEntity save(OAuth2User userDto, AuthProvider provider) {
        return switch (provider) {
            case GITHUB -> this.saveUserFromGithab(userDto);
            case GOOGLE -> this.saveUserFromGoogle(userDto);
            case YANDEX -> this.saveUserFromYandex(userDto);
        };
    }

    @Override
    public AuthorizedUser saveAndMap(OAuth2User userDto, AuthProvider provider) {
        UserEntity entity = this.save(userDto, provider);
        return AuthorizedUserMapper.map(entity, provider);
    }

    private UserEntity saveUserFromGithab(OAuth2User userDto) {
        String email = userDto.getAttribute("email");
        if (email == null) {
            throw new AuthException(AuthErrorCode.EMAIL_IS_EMPTY);
        }
        UserEntity user = this.userRepository.findByEmail(email);
        if (user == null) {
            user = new UserEntity();
            user.setEmail(email);
            user.setActive(true);
        }

        if (userDto.getAttribute("name") != null) {
            String[] splitted = ((String) userDto.getAttribute("name")).split(" ");
            user.setFirstName(splitted[0]);
            if (splitted.length > 1) {
                user.setSecondName(splitted[1]);
            }
            if (splitted.length > 2) {
                user.setMiddleName(splitted[2]);
            }
        } else {
            user.setFirstName(userDto.getAttribute("login"));
            user.setSecondName(userDto.getAttribute("login"));
        }

        if (userDto.getAttribute("avatar_url") != null) {
            user.setAvatarUrl(userDto.getAttribute("avatar_url"));
        }
        return userRepository.save(user);
    }

    private UserEntity saveUserFromGoogle(OAuth2User userDto) {
        String email = userDto.getAttribute("email");
        if (email == null) {
            throw new AuthException(AuthErrorCode.EMAIL_IS_EMPTY);
        }
        UserEntity user = this.userRepository.findByEmail(email);
        if (user == null) {
            user = new UserEntity();
            user.setEmail(email);
            user.setActive(true);
        }

        if (userDto.getAttribute("given_name") != null) {
            user.setFirstName(userDto.getAttribute("given_name"));
        }

        if (userDto.getAttribute("family_name") != null) {
            user.setSecondName(userDto.getAttribute("family_name"));
        }

        if (userDto.getAttribute("picture") != null) {
            user.setAvatarUrl(userDto.getAttribute("picture"));
        }

        return userRepository.save(user);
    }

    private UserEntity saveUserFromYandex(OAuth2User userDto) {
        String email = userDto.getAttribute("default_email");
        if (email == null) {
            throw new AuthException(AuthErrorCode.EMAIL_IS_EMPTY);
        }
        UserEntity user = this.userRepository.findByEmail(email);
        if (user == null) {
            user = new UserEntity();
            user.setEmail(email);
            user.setActive(true);
        }

        if (userDto.getAttribute("first_name") != null) {
            user.setFirstName(userDto.getAttribute("first_name"));
        }

        if (userDto.getAttribute("last_name") != null) {
            user.setSecondName(userDto.getAttribute("last_name"));
        }

        if (userDto.getAttribute("default_avatar_id") != null) {
            user.setAvatarUrl(this.createYandexAvatarUrl(userDto.getAttribute("default_avatar_id")));
        }
        if (userDto.getAttribute("birthday") != null) {
            user.setBirthday(LocalDate.parse(userDto.getAttribute("birthday"), DateTimeFormatter.ISO_LOCAL_DATE));
        }

        return userRepository.save(user);
    }

    private String createYandexAvatarUrl(String avatarId) {
        return yandexAvatarUrl.replace("{avatarId}", avatarId);
    }
}