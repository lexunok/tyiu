package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.RatingService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
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
        return ratingService.getRatings(ideaId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not Found")));
    }

    @GetMapping("/{ideaId}")
    @PreAuthorize("hasAuthority('EXPERT')")
    public Mono<RatingDTO> getExpertRatingForIdea(@PathVariable Long ideaId, Principal principal){
        return ratingService.getExpertRating(ideaId, Long.valueOf(principal.getName()))
                .switchIfEmpty(Mono.error(new NotFoundException("Not Found")));
    }

    @PutMapping("/save")
    @PreAuthorize("hasAuthority('EXPERT')")
    public Mono<InfoResponse> saveRating(@RequestBody RatingDTO ratingDTO, Principal principal) {
        return ratingService.saveRating(ratingDTO, Long.valueOf(principal.getName()))
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success saving!"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST, "Not saved"));
    }

    @PutMapping("/confirm")
    @PreAuthorize("hasAuthority('EXPERT')")
    public Mono<InfoResponse> confirmRating(@RequestBody RatingDTO ratingDTO, Principal principal) {
        return ratingService.confirmRating(ratingDTO, Long.valueOf(principal.getName()))
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success confirming!"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Not confirmed"));
    }
}
