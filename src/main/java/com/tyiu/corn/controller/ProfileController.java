package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.entities.Profile;
import com.tyiu.corn.model.requests.ProfileUpdateRequest;
import com.tyiu.corn.service.ProfileService;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class ProfileController {
    private final ProfileService profileService;

    @GetMapping("/{id}")
    public Mono<ProfileDTO> getProfile(@PathVariable String profileId) {
        return profileService.getProfile(profileId);
    }

    @PostMapping("/create")
    public Mono<ProfileDTO> createProfile(@RequestBody ProfileDTO profile) {
        return profileService.createProfile(profile);
    }

    @PutMapping("/update/{id}")
    public Mono<ResponseEntity<String>> updateProfile(@PathVariable String profileId, @RequestBody ProfileUpdateRequest request) {
        profileService.updateProfile(profileId, request);
        return Mono.just(new ResponseEntity<>("Success updating", HttpStatus.OK));
    }
}
