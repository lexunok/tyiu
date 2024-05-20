package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.ProfileDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.service.ProfileService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
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
                                           @AuthenticationPrincipal User user) {
        return profileService.getUserProfile(userId, user.getId());
    }

    @PostMapping("/skills/save")
    public Flux<SkillDTO> saveUserSkills(@AuthenticationPrincipal User user, @RequestBody Flux<SkillDTO> skills) {
        return profileService.saveSkills(user.getId(), skills);
    }
}
