package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.ProfileUpdateRequest;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.connections.ProfileClient;
import com.tyiu.client.exceptions.MediaException;
import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.client.exceptions.ServerProcessException;
import com.tyiu.client.models.Role;
import com.tyiu.client.models.UserDTO;
import jakarta.servlet.http.Part;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ProfileService {

    private static final long MAX_FILE_SIZE_BYTES = 16 * 1024 * 1024;
    private final UserRepository userRepository;
    private final ProfileClient profileClient;
    private final ModelMapper mapper;
    private final RedisTemplate<String, Object> template;

    @Value("${file.path}")
    String path;

    public List<UserDTO> getAllUsers(){
        return userRepository.findByIsDeletedFalse().stream().map(u ->
                UserDTO.builder()
                        .id(u.getId())
                        .roles(u.getRoles())
                        .email(u.getEmail())
                        .firstName(u.getFirstName())
                        .lastName(u.getLastName())
                        .createdAt(u.getCreatedAt().toString())
                        .studyGroup(u.getStudyGroup())
                        .telephone(u.getTelephone())
                        .build())
                .toList();
    }

    public UserDTO getUser(String id){
        User u = userRepository.findById(id).orElseThrow(() -> new NotFoundException("Not found"));
        return UserDTO.builder()
                .id(u.getId())
                .roles(u.getRoles())
                .email(u.getEmail())
                .telephone(u.getTelephone())
                .studyGroup(u.getStudyGroup())
                .firstName(u.getFirstName())
                .lastName(u.getLastName())
                .createdAt(u.getCreatedAt().toString())
                .build();
    }

    public Resource getAvatar(String userId){
        //Path avatarPath = Paths.get(path, userId + "_avatar.jpg");
        String volumePath = System.getProperty("user.home") + "/avatars";
        Path avatarPath = Paths.get(volumePath, userId + "_avatar.jpg");
        if (!Files.exists(avatarPath)){
            throw new NotFoundException("Аватар не найден");
        }
        return new FileSystemResource(avatarPath);
    }

    public FileSystemResource uploadAvatar(String userId, Part file) {
        Path basePath = Paths.get(path);
        Path avatarPath = basePath.resolve(userId + "_avatar.jpg");

        if (!(MediaType.IMAGE_JPEG_VALUE.equals(file.getContentType()) || MediaType.IMAGE_PNG_VALUE.equals(file.getContentType()))
                || file.getSize() > MAX_FILE_SIZE_BYTES) {
            throw new MediaException("Недопустимый тип или размер файла");
        }

        try (InputStream fileStream = file.getInputStream()) {
            Files.createDirectories(basePath);
            Files.copy(fileStream, avatarPath, StandardCopyOption.REPLACE_EXISTING);
            return new FileSystemResource(avatarPath);
        } catch (Exception e) {
            throw new ServerProcessException("Ошибка загрузки аватара");
        }
    }

    public void updateProfile(String id, ProfileUpdateRequest request){
        userRepository.findById(id)
                .ifPresent(u -> template.opsForHash().delete("user", u.getEmail().toLowerCase()));
        userRepository.updateProfileById(request.getFirstName(), request.getLastName(),
                request.getTelephone(), request.getStudyGroup(), id);
        profileClient.checkUser(mapper.map(userRepository.findById(id), UserDTO.class));
    }

    public UserDTO updateUserByAdmin(UserDTO userDTO){
        if (userRepository.existsById(userDTO.getId())) {
            userRepository.findById(userDTO.getId())
                    .ifPresent(u -> template.opsForHash().delete("user", u.getEmail().toLowerCase()));

            userRepository.updateProfileForAdminById(userDTO.getFirstName(), userDTO.getLastName(),
                    userDTO.getEmail(), userDTO.getRoles(), userDTO.getTelephone(), userDTO.getStudyGroup(), userDTO.getId());
            profileClient.checkUser(mapper.map(userRepository.findById(userDTO.getId()), UserDTO.class));
            return userDTO;
        }
        else throw new NotFoundException("Пользователя не существует");
    }

    public void changeTeamLeader(String teamLeaderId, String userId){
        User oldTeamLeader = userRepository.findById(teamLeaderId).orElseThrow(() -> new NotFoundException("Not found"));
        oldTeamLeader.getRoles().remove(Role.TEAM_LEADER);
        userRepository.save(oldTeamLeader);
        template.opsForHash().delete("user", oldTeamLeader.getEmail().toLowerCase());
        User newTeamLeader = userRepository.findById(userId).orElseThrow(() -> new NotFoundException("Not found"));
        newTeamLeader.getRoles().add(Role.TEAM_LEADER);
        userRepository.save(newTeamLeader);
        template.opsForHash().delete("user", newTeamLeader.getEmail().toLowerCase());
    }

    public void deleteUser(String id){
        userRepository.setUserIsDeletedById(Boolean.TRUE, id);
    }

}
