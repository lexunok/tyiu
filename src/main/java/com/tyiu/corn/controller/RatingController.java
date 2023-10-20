package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.service.RatingService;
import com.tyiu.corn.util.CustomUserDetails;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;

@RestController
@RequestMapping("/api/v1/rating")
@RequiredArgsConstructor
public class RatingController {

    private final RatingService ratingService;

    @GetMapping("/all/{ideaId}")
    public Flux<RatingDTO> getAllIdeasRatings(@PathVariable Long ideaId){
        return ratingService.getRatings(ideaId);
    }

    @GetMapping("/{ideaId}")
    public Mono<RatingDTO> getExpertRatingForIdea(@PathVariable Long ideaId,
                                                  @AuthenticationPrincipal CustomUserDetails user){
        return ratingService.getExpertRating(ideaId, user.getId());
    }

    @PutMapping("/save")
    public Mono<ResponseEntity<String>> saveRating(@RequestBody RatingDTO ratingDTO,
                                                   @AuthenticationPrincipal CustomUserDetails user) {
        return ratingService.saveRating(ratingDTO,  user.getId())
                .thenReturn(ResponseEntity.ok("Success saving!"));
    }

    @PutMapping("/confirm")
    public Mono<ResponseEntity<String>> confirmRating(@RequestBody RatingDTO ratingDTO,
                                                      @AuthenticationPrincipal CustomUserDetails user) {
        return ratingService.confirmRating(ratingDTO, user.getId())
                .thenReturn(ResponseEntity.ok("Success confirming!"));
    }
}
