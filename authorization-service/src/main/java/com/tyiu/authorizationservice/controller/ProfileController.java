package com.tyiu.authorizationservice.controller;


import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.ProfileUpdateRequest;
import com.tyiu.authorizationservice.service.ProfileService;
import com.tyiu.client.exceptions.ServerProcessException;
import com.tyiu.client.models.UserDTO;
import jakarta.servlet.http.Part;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Base64;
import java.util.List;

@RestController
@RequestMapping("/api/v1/authorization-service/profile")
@RequiredArgsConstructor
public class ProfileController {

    private final ProfileService profileService;

    @GetMapping
    public User getJwt(@AuthenticationPrincipal User user){
        return user;
    }

    @GetMapping("/users/all")
    public List<UserDTO> getAllUsers() {
        return profileService.getAllUsers();
    }

    @GetMapping("/users/all/email")
    public List<String> getAllUserEmails(){
        return profileService.getAllUserEmails();
    }

    //TODO: нужно проверить
    @GetMapping("/avatar/get/{userId}")
    public ResponseEntity<String> getAvatar(@PathVariable String userId) {
        try {
            return ResponseEntity
                    .ok()
                    .contentType(MediaType.IMAGE_JPEG)
                    .body(Base64.getEncoder().encodeToString(profileService.getAvatar(userId).getContentAsByteArray()));
        }
        catch (Exception e) {
            throw new ServerProcessException("Ошибка при получении аватара");
        }
    }

    //TODO: нужно проверить
    @PostMapping("/avatar/upload")
    public ResponseEntity<FileSystemResource> uploadAvatar(@AuthenticationPrincipal User user,
                                                           @RequestPart("file") Part file) {
        return ResponseEntity
                .ok()
                .contentType(MediaType.IMAGE_JPEG)
                .body(profileService.uploadAvatar(user.getId(),file));
    }

    @PutMapping("/{id}")
    public void updateProfile(@RequestBody ProfileUpdateRequest request, @AuthenticationPrincipal User user) {
        profileService.updateProfile(user.getId(), request);
    }

    @PutMapping("/user/{id}")
    public UserDTO updateUserByAdmin(@RequestBody UserDTO user) {
        return profileService.updateUserByAdmin(user);
    }

    @DeleteMapping("/user/{id}")
    public void deleteUser(@PathVariable String id) {
        profileService.deleteUser(id);
    }

}
