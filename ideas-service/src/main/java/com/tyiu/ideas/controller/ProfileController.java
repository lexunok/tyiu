package com.tyiu.ideas.controller;

import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.dto.ProfileDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.service.ProfileService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/ideas-service/profile")
@RequiredArgsConstructor
public class ProfileController {

    private final ProfileService profileService;

    @GetMapping("/{userId}")
    public Mono<ProfileDTO> getUserProfile(@PathVariable String userId,
                                           @AuthenticationPrincipal Jwt user) {
        return profileService.getUserProfile(userId, user.getId());
    }

    @PostMapping
    public Mono<Void> checkUser(@RequestBody UserDTO userDTO) {
        return profileService.checkUser(userDTO);
    }

    @PostMapping("/skills/save")
    public Flux<SkillDTO> saveUserSkills(@AuthenticationPrincipal Jwt user, @RequestBody Flux<SkillDTO> skills) {
        return profileService.saveSkills(user.getId(), skills);
    }
}
