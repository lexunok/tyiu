package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.service.ProfileService;

import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class ProfileController {

    private final ProfileService profileService;


    @GetMapping("/{email}")
    public Mono<ProfileDTO> getUserProfile(@PathVariable String email) {
        return profileService.getUserProfile(email);
    }


    @GetMapping("/avatar/get/{email}")
    public Mono<ResponseEntity<Resource>> getAvatar(@PathVariable String email) {
        return profileService.getAvatar(email)
                .map(avatar -> ResponseEntity
                        .ok()
                        .contentType(MediaType.IMAGE_JPEG)
                        .body(avatar));
    }

    @PostMapping("/avatar/upload")
    public Mono<ResponseEntity<Resource>> uploadAvatar(@AuthenticationPrincipal User user,
                                         @RequestPart("file") FilePart file) {
        return Mono.just(ResponseEntity
                        .ok()
                        .contentType(MediaType.IMAGE_JPEG)
                        .body(profileService.uploadAvatar(user.getId(),file)));
    }

    @PostMapping("/skills/save")
    public Flux<SkillDTO> saveUserSkills(Principal principal,
                                                     @RequestBody Flux<SkillDTO> skills) {
        return profileService.saveSkills(principal.getName(), skills);
    }
}
