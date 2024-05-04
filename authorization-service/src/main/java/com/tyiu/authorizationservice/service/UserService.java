package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@RequiredArgsConstructor
@Service
public class UserService implements UserDetailsService {
    private final UserRepository repository;
    private final RedisTemplate<String, Object> template;

    @Override
    public UserDetails loadUserByUsername(String email) {
        User user = (User) template.opsForHash().get("user", email);
        if (user == null) {
            user = repository.findByEmail(email.toLowerCase())
                    .orElseThrow(() -> new UsernameNotFoundException("Пользователь не найден!"));
            template.opsForHash().put("user", email, user);
        }
        return user;
    }

}
