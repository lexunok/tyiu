package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.service.RatingService;
import lombok.RequiredArgsConstructor;
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
    public Flux<RatingDTO> getAllIdeasRatings(@PathVariable String ideaId){
        return ratingService.getRatings(ideaId);
    }

    @GetMapping("/{ideaId}")
    public Mono<RatingDTO> getExpertRatingForIdea(@PathVariable String ideaId, Principal principal){
        return ratingService.getExpertRating(ideaId, principal.getName());
    }

    @PutMapping("/save/{ideaId}")
    public Mono<Void> saveRating(@RequestBody RatingDTO rating, Principal principal, @PathVariable String ideaId) {
        return ratingService.saveRating(rating, principal.getName(), ideaId);
    }

    @PutMapping("/confirm/{ideaId}")
    public Mono<Void> confirmRating(@RequestBody RatingDTO rating, Principal principal, @PathVariable String ideaId) {
        return ratingService.confirmRating(rating, principal.getName(), ideaId);
    }
}
