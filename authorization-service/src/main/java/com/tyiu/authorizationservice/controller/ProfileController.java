package com.tyiu.authorizationservice.controller;


import com.tyiu.authorizationservice.model.entity.User;
import com.tyiu.authorizationservice.model.request.ProfileUpdateRequest;
import com.tyiu.authorizationservice.service.ProfileService;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

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

    @GetMapping("/users/all/email")
    public List<String> getAllUserEmails(){
        return profileService.getAllUserEmails();
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
