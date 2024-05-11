package com.tyiu.authorizationservice.controller;


import com.tyiu.authorizationservice.service.ProfileService;
import com.tyiu.client.models.UserDTO;
import lombok.RequiredArgsConstructor;
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

    @DeleteMapping("/user/{id}")
    public void deleteUser(@PathVariable String id) {
        profileService.deleteUser(id);
    }

}
