package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.entities.User;
import com.tyiu.authorizationservice.model.mappers.AuthorizedUserMapper;
import com.tyiu.authorizationservice.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        User entity = userRepository.findUserByEmail(username);
        if (entity == null) {
            throw new UsernameNotFoundException("User with username = " + username + " not found");
        }
        return AuthorizedUserMapper.map(entity, null);
    }
}
