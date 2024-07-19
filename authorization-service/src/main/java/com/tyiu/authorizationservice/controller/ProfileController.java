package com.tyiu.authorizationservice.controller;

import com.tyiu.authorizationservice.model.request.ProfileUpdateRequest;
import com.tyiu.authorizationservice.service.ProfileService;
import com.tyiu.client.models.UserDTO;
import jakarta.servlet.http.Part;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;

import java.util.Base64;
import java.util.List;

@RestController
@RequestMapping("/api/v1/authorization-service/profile")
@RequiredArgsConstructor
public class ProfileController {

    private final ProfileService profileService;

    @GetMapping("/users/all")
    public List<UserDTO> getAllUsers() {
        return profileService.getAllUsers();
    }

    @GetMapping("/{id}")
    public UserDTO getProfile(@PathVariable String id) {
        return profileService.getUser(id);
    }

    @GetMapping("/avatar/get/{userId}")
    public ResponseEntity<String> getAvatar(@PathVariable String userId) {
        try {
            return ResponseEntity
                    .ok()
                    .contentType(MediaType.IMAGE_JPEG)
                    .body(Base64.getEncoder().encodeToString(profileService.getAvatar(userId).getContentAsByteArray()));
        }
        catch (Exception e) {
            return ResponseEntity
                    .ok()
                    .contentType(MediaType.IMAGE_JPEG)
                    .body("");
            //throw new ServerProcessException("Ошибка при получении аватара");
        }
    }

    @PostMapping("/avatar/upload")
    public ResponseEntity<FileSystemResource> uploadAvatar(@AuthenticationPrincipal Jwt user,
                                                           @RequestPart("file") Part file) {
        return ResponseEntity
                .ok()
                .contentType(MediaType.IMAGE_JPEG)
                .body(profileService.uploadAvatar(user.getId(),file));
    }

    @PutMapping("/update")
    public void updateProfile(@RequestBody ProfileUpdateRequest request, @AuthenticationPrincipal Jwt user) {
        profileService.updateProfile(user.getId(), request);
    }

    @PreAuthorize("hasRole('ADMIN')")
    @PutMapping("/user/{id}")
    public UserDTO updateUserByAdmin(@RequestBody UserDTO user) {
        return profileService.updateUserByAdmin(user);
    }

    @PreAuthorize("hasAnyRole('TEAM_OWNER','ADMIN')")
    @PutMapping("/leader/{teamLeaderId}/{userId}")
    public void updateTeamLeader(@PathVariable String teamLeaderId,
                                 @PathVariable String userId) {
        profileService.changeTeamLeader(teamLeaderId, userId);
    }

    @PreAuthorize("hasRole('ADMIN')")
    @DeleteMapping("/user/{id}")
    public void deleteUser(@PathVariable String id) {
        profileService.deleteUser(id);
    }

}
