package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.CustomHttpException;
import com.tyiu.ideas.model.dto.ProfileDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.dto.UserDTO;
import com.tyiu.ideas.model.requests.ProfileUpdateRequest;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.ProfileService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
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

    @GetMapping("/{userId}")
    public Mono<ProfileDTO> getUserProfile(@PathVariable String userId,
                                           @AuthenticationPrincipal Jwt jwt) {
        return profileService.getUserProfile(userId, jwt.getId());
    }

    @PostMapping("/avatar/upload")
    public Mono<ResponseEntity<Mono<FileSystemResource>>> uploadAvatar(@AuthenticationPrincipal Jwt jwt,
                                                                       @RequestPart("file") FilePart file) {
        return Mono.just(ResponseEntity
                .ok()
                .contentType(MediaType.IMAGE_JPEG)
                .body(profileService.uploadAvatar(jwt.getId(),file)));
    }

    @PostMapping("/skills/save")
    public Flux<SkillDTO> saveUserSkills(@AuthenticationPrincipal Jwt jwt, @RequestBody Flux<SkillDTO> skills) {
        return profileService.saveSkills(jwt.getId(), skills);
    }

    @DeleteMapping("/delete/user/{userId}")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<Void> deleteUser(@PathVariable String userId){
        return profileService.deleteUser(userId);
    }

    @PutMapping("/update")
    public Mono<InfoResponse> updateProfile(@AuthenticationPrincipal Jwt jwt, @RequestBody ProfileUpdateRequest request) {
        return profileService.updateProfile(jwt.getId(), request)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное изменение"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST, "Не удалось изменить"));
    }

    @PutMapping("/change/info")
    @PreAuthorize("hasAuthority('ADMIN')")
    public Mono<UserDTO> changeUserInfoByAdmin(@RequestBody UserDTO user){
        return profileService.changeUserInfo(user)
                .switchIfEmpty(Mono.error(new CustomHttpException("Не удалось изменить пользователя", HttpStatus.INTERNAL_SERVER_ERROR.value())));
    }
}
