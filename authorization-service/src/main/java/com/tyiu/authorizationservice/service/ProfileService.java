package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.ProfileUpdateRequest;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ProfileService {

    private final UserRepository userRepository;
    private final ModelMapper mapper;

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

    public void updateProfile(String id, ProfileUpdateRequest request){
        userRepository.updateProfileById(request.getFirstName(), request.getLastName(),
                request.getTelephone(), request.getStudyGroup(), id);
    }

    public UserDTO updateUserByAdmin(UserDTO userDTO){
        if (userRepository.existsById(userDTO.getId())) {
            User user = userRepository.save(mapper.map(userDTO, User.class));
            return mapper.map(user, UserDTO.class);
        }
        else throw new NotFoundException("Пользователя не существует");
    }

    public void deleteUser(String id){
        userRepository.setUserIsDeletedById(Boolean.TRUE, id);
    }

}
