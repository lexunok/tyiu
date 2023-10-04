package com.tyiu.corn.controller;

import com.mongodb.client.gridfs.model.GridFSFile;
import com.tyiu.corn.model.dto.ProfileDTO;
import com.tyiu.corn.model.requests.ProfileUpdateRequest;
import com.tyiu.corn.model.requests.UserSkillRequest;
import com.tyiu.corn.model.responses.UserSkillResponse;
import com.tyiu.corn.service.ProfileService;

import lombok.RequiredArgsConstructor;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;
import java.util.List;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class ProfileController {
    private final ProfileService profileService;

    @GetMapping("/")
    public Mono<ProfileDTO> getProfile(Principal principal) {
        return profileService.getProfile(principal.getName());
    }

    @PostMapping("/upload/avatar")
    public Mono<GridFSFile> uploadAvatar(Principal principal,
                                         @RequestPart("file") FilePart filePart) {
        return profileService.uploadAvatar(principal.getName(), filePart);
    }
    @GetMapping("/get/skills")
    public Flux<UserSkillResponse> getUserSkills(Principal principal) {
        return profileService.getSkills(principal.getName());
    }
    @PostMapping("/save/skills")
    public Flux<UserSkillResponse> saveUserSkills(Principal principal,
                                                  @RequestBody Flux<UserSkillRequest> skills) {
        return profileService.saveSkills(principal.getName(), skills);
    }
}
