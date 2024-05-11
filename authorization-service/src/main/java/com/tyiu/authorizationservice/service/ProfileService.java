package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ProfileService {

    private final UserRepository userRepository;

    public List<UserDTO> getAllUsers(){
        return userRepository.findByIsDeletedFalse().stream().map(u ->
                UserDTO.builder()
                        .id(u.getId())
                        .roles(u.getRoles())
                        .email(u.getEmail())
                        .firstName(u.getFirstName())
                        .lastName(u.getLastName())
                        .createdAt(u.getCreatedAt())
                        .build())
                .toList();
    }

    public List<String> getAllUserEmails(){
        return userRepository.findAllEmails();
    }

    public void deleteUser(String id){
        userRepository.setUserIsDeletedById(Boolean.TRUE, id);
    }

}
