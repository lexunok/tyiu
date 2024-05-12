package com.tyiu.authorizationservice.service;

import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.ProfileUpdateRequest;
import com.tyiu.authorizationservice.repository.UserRepository;
import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.client.exceptions.ServerProcessException;
import com.tyiu.client.models.UserDTO;
import jakarta.servlet.http.Part;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
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

    private boolean isValidImageFile(Part file) {
        String contentType = file.getContentType();
        return "image/jpeg".equals(contentType) || "image/png".equals(contentType);
    }

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

    public Resource getAvatar(String userId){
        try {
            return new FileSystemResource(Paths.get(path, userId + "_avatar.jpg"));
        } catch (Exception e) {
            throw new NotFoundException("Аватар не найден");
        }
    }

    public FileSystemResource uploadAvatar(String userId, Part file) {
        Path basePath = Paths.get(path);
        Path avatarPath = basePath.resolve(userId + "_avatar.jpg");

        if (!isValidImageFile(file) || file.getSize() > MAX_FILE_SIZE_BYTES) {
            throw new ServerProcessException("Недопустимый тип или размер файла");
        }

        try {
            Files.createDirectories(basePath);
            InputStream fileStream = file.getInputStream();
            Files.copy(fileStream, avatarPath, StandardCopyOption.REPLACE_EXISTING);
            fileStream.close();
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
