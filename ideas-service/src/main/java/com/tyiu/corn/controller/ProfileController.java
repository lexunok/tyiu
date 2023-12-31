package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.requests.ProfileUpdateRequest;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.ProfileService;

import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.security.Principal;
import java.util.Base64;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class ProfileController {

    private final ProfileService profileService;


    @GetMapping("/{userId}")
    public Mono<ProfileDTO> getUserProfile(@PathVariable String userId) {
        return profileService.getUserProfile(userId);
    }

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
    public Mono<ResponseEntity<Resource>> uploadAvatar(@AuthenticationPrincipal User user,
                                                       @RequestPart("file") FilePart file) {
        return Mono.just(ResponseEntity
                        .ok()
                        .contentType(MediaType.IMAGE_JPEG)
                        .body(profileService.uploadAvatar(user.getId(),file)));
    }

    @PostMapping("/skills/save")
    public Flux<SkillDTO> saveUserSkills(Principal principal, @RequestBody Flux<SkillDTO> skills) {
        return profileService.saveSkills(principal.getName(), skills);
    }
    @PutMapping("/fullname/update")
    public Mono<InfoResponse> updateUserFullName(@AuthenticationPrincipal User user, @RequestBody ProfileUpdateRequest request) {
        return profileService.updateFullName(user.getId(), request)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное изменение"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST, "Не удалось изменить"));
    }
}
