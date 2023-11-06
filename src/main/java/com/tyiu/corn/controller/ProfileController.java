package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.UserDTO;
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

    @GetMapping("/ideas/get/{email}")
    public Flux<ProfileIdeaResponse> getUserIdeas(@PathVariable String email) {
        return profileService.getUserIdeas(email);
    }

    @GetMapping("/projects/get/{email}")
    public Flux<ProfileProjectResponse> getUserProjects(@PathVariable String email) {
        return profileService.getUserProjects(email);
    }

    @GetMapping("/{email}")
    public Mono<UserDTO> getUserProfile(@PathVariable String email) {
        return profileService.getUserProfile(email);
    }


    @GetMapping("/avatar/get/{email}")
    public Mono<Resource> getAvatar(@PathVariable String email) {
        return profileService.getAvatar(email);
    }

    @PostMapping("/avatar/upload")
    public Mono<Resource> uploadAvatar(Principal principal,
                                         @RequestPart("file") FilePart file) {
        return profileService.uploadAvatar(principal.getName(), file);
    }
    @GetMapping("/skills/get/{email}")
    public Flux<ProfileSkillResponse> getUserSkills(@PathVariable String email) {
        return profileService.getSkills(email);
    }
    @PostMapping("/skills/save")
    public Flux<SkillDTO> saveUserSkills(Principal principal,
                                                     @RequestBody Flux<SkillDTO> skills) {
        return profileService.saveSkills(Long.valueOf(principal.getName()), skills);
    }
}
