package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.ProfileUpdateRequest;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.exceptions.MediaException;
import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.client.exceptions.ServerProcessException;
import com.tyiu.client.models.UserDTO;
import jakarta.servlet.http.Part;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
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
    private final ModelMapper mapper;

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
                        .build())
                .toList();
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
        //Path basePath = Paths.get(path);
        //Path avatarPath = basePath.resolve(userId + "_avatar.jpg");

        String volumePath = System.getProperty("user.home") + "/avatars";
        Path avatarPath = Paths.get(volumePath).resolve(userId + "_avatar.jpg");

        if (!(MediaType.IMAGE_JPEG_VALUE.equals(file.getContentType()) || MediaType.IMAGE_PNG_VALUE.equals(file.getContentType()))
                || file.getSize() > MAX_FILE_SIZE_BYTES) {
            throw new MediaException("Недопустимый тип или размер файла");
        }

        try (InputStream fileStream = file.getInputStream()) {
            //Files.createDirectories(basePath);
            Files.createDirectories(avatarPath.getParent());
            //Files.copy(fileStream, avatarPath, StandardCopyOption.REPLACE_EXISTING);
            Files.copy(fileStream, avatarPath, StandardCopyOption.REPLACE_EXISTING);
            return new FileSystemResource(avatarPath);
        } catch (Exception e) {
            throw new ServerProcessException("Ошибка загрузки аватара");
        }
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
