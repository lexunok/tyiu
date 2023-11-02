package com.tyiu.corn.controller;

import com.tyiu.corn.model.requests.ProfileSkillRequest;
import com.tyiu.corn.model.responses.ProfileIdeaResponse;
import com.tyiu.corn.model.responses.ProfileSkillResponse;
import com.tyiu.corn.model.responses.ProfileProjectResponse;
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
    public Flux<ProfileIdeaResponse> getUserIdeas(Principal principal) {
        return profileService.getUserIdeas(Long.valueOf(principal.getName()));
    }

    @GetMapping("/projects/get")
    public Flux<ProfileProjectResponse> getUserProjects(Principal principal) {
        return profileService.getUserProjects(Long.valueOf(principal.getName()));
    }

    //@GetMapping("/my")
    //public Mono<ProfileDTO> getProfile(Principal principal){
    //    return profileService.getProfile(Long.valueOf(principal.getName()));
    //}


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
    public Flux<ProfileSkillResponse> getUserSkills(Principal principal) {
        return profileService.getSkills(Long.valueOf(principal.getName()));
    }
    @PostMapping("/skills/save")
    public Flux<ProfileSkillResponse> saveUserSkills(Principal principal,
                                                     @RequestBody Flux<ProfileSkillRequest> skills) {
        return profileService.saveSkills(Long.valueOf(principal.getName()), skills);
    }
}
