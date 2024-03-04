package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.ProfileDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.requests.ProfileUpdateRequest;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.ProfileService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.util.Base64;

@RestController
@RequestMapping("/api/v1/ideas-service/profile")
@RequiredArgsConstructor
public class ProfileController {

    private final ProfileService profileService;
    @GetMapping("/avatar/get/{userId}")
    public Mono<ResponseEntity<String>> getAvatar(@PathVariable String userId) {
        return profileService.getAvatar(userId)
                .flatMap(avatar -> {
                    try {
                        String image = Base64.getEncoder().encodeToString(avatar.getContentAsByteArray());
                        return Mono.just(ResponseEntity
                                .ok()
                                .contentType(MediaType.IMAGE_JPEG)
                                .body(image));
                    }
                    catch (IOException e) {
                        return Mono.empty();
                    }
                });
    }

    @PostMapping("/avatar/upload")
    public Mono<ResponseEntity<Mono<FileSystemResource>>> uploadAvatar(@AuthenticationPrincipal User user,
                                                                       @RequestPart("file") FilePart file) {
        return Mono.just(ResponseEntity
                .ok()
                .contentType(MediaType.IMAGE_JPEG)
                .body(profileService.uploadAvatar(user.getId(),file)));
    }

    @GetMapping("/{userId}")
    public Mono<ProfileDTO> getUserProfile(@PathVariable String userId,
                                           @AuthenticationPrincipal User user) {
        return profileService.getUserProfile(userId, user.getId());
    }

    @PostMapping("/skills/save")
    public Flux<SkillDTO> saveUserSkills(@AuthenticationPrincipal User user, @RequestBody Flux<SkillDTO> skills) {
        return profileService.saveSkills(user.getId(), skills);
    }
    @PutMapping("/update/{userId}")
    public Mono<InfoResponse> updateProfile(@RequestBody ProfileUpdateRequest request, @PathVariable String userId) {
        return profileService.updateProfile(userId, request)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное изменение"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST, "Не удалось изменить"));
    }
}
