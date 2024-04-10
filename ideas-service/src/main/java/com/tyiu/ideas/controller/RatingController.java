package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.dto.RatingDTO;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.RatingService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@RestController
@RequestMapping("/api/v1/ideas-service/rating")
@RequiredArgsConstructor
public class RatingController {

    private final RatingService ratingService;

    @GetMapping("/all/{ideaId}")
    @PreAuthorize("hasRole('EXPERT') || hasRole('TEACHER') || hasRole('PROJECT_OFFICE') || hasRole('ADMIN')")
    public Flux<RatingDTO> getAllIdeasRatings(@PathVariable String ideaId){
        return ratingService.getRatings(ideaId);
    }

    @GetMapping("/{ideaId}")
    @PreAuthorize("hasRole('EXPERT') || hasRole('TEACHER') || hasRole('PROJECT_OFFICE') || hasRole('ADMIN')")
    public Mono<RatingDTO> getExpertRatingForIdea(@PathVariable String ideaId, @AuthenticationPrincipal Jwt jwt){
        return ratingService.getExpertRating(ideaId, jwt.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Рейтинг не найден")));
    }

    @PutMapping("/save")
    @PreAuthorize("hasRole('EXPERT') || hasRole('PROJECT_OFFICE') || hasRole('ADMIN')")
    public Mono<InfoResponse> saveRating(@RequestBody RatingDTO ratingDTO, @AuthenticationPrincipal Jwt jwt) {
        return ratingService.saveRating(ratingDTO, jwt.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное сохранение рейтинга"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST, "Рейтинг не сохранен"));
    }

    @PutMapping("/confirm")
    @PreAuthorize("hasRole('EXPERT') || hasRole('PROJECT_OFFICE') || hasRole('ADMIN')")
    public Mono<InfoResponse> confirmRating(@RequestBody RatingDTO ratingDTO, @AuthenticationPrincipal Jwt jwt) {
        return ratingService.confirmRating(ratingDTO, jwt.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное утверждение рейтинга"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Рейтинг не утвержден"));
    }
}
