package com.tyiu.corn.controller;

import com.tyiu.corn.model.requests.UserSkillRequest;
import com.tyiu.corn.model.responses.UserIdeaResponse;
import com.tyiu.corn.model.responses.UserProjectResponse;
import com.tyiu.corn.model.responses.UserSkillResponse;
import com.tyiu.corn.service.ProfileService;

import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
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

    @GetMapping("/ideas/get")
    public Flux<UserIdeaResponse> getUserIdeas(Principal principal) {
        return profileService.getUserIdeas(principal.getName());
    }
    @GetMapping("/projects/get")
    public Flux<UserProjectResponse> getUserProjects(Principal principal) {
        return profileService.getUserProjects(principal.getName());
    }


    @GetMapping("/avatar/get")
    public Mono<Resource> getAvatar(Principal principal) {
        return profileService.getAvatar(principal.getName());
    }

    @PostMapping("/avatar/upload")
    public Mono<Resource> uploadAvatar(Principal principal,
                                         @RequestPart("file") FilePart file) {
        return profileService.uploadAvatar(principal.getName(), file);
    }
    @GetMapping("/skills/get")
    public Flux<UserSkillResponse> getUserSkills(Principal principal) {
        return profileService.getSkills(principal.getName());
    }
    @PostMapping("/skills/save")
    public Flux<UserSkillResponse> saveUserSkills(Principal principal,
                                                  @RequestBody Flux<UserSkillRequest> skills) {
        return profileService.saveSkills(principal.getName(), skills);
    }
}
