package com.tyiu.corn.controller;

import com.mongodb.client.gridfs.model.GridFSFile;
import com.tyiu.corn.model.requests.UserSkillRequest;
import com.tyiu.corn.model.responses.UserIdeaResponse;
import com.tyiu.corn.model.responses.UserSkillResponse;
import com.tyiu.corn.service.ProfileService;

import lombok.RequiredArgsConstructor;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.data.mongodb.gridfs.ReactiveGridFsResource;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;

@RestController
@RequestMapping("/api/v1/profile")
@RequiredArgsConstructor
public class ProfileController {

    private final ProfileService profileService;

    @GetMapping("/get/ideas")
    public Flux<UserIdeaResponse> getUserIdeas(Principal principal) {
        return profileService.getUserIdeas(principal.getName());
    }

    @GetMapping("/get/avatar")
    public Flux<DataBuffer> getAvatar(Principal principal) {
        return profileService.getAvatar(principal.getName());
    }

    @PostMapping("/upload/avatar")
    public Flux<DataBuffer> uploadAvatar(Principal principal,
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
