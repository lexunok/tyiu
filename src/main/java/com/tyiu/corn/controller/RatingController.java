package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.RatingDTO;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.RatingService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@RestController
@RequestMapping("/api/v1/rating")
@RequiredArgsConstructor
public class RatingController {

    private final RatingService ratingService;

    @GetMapping("/all/{ideaId}")
    @PreAuthorize("hasAuthority('EXPERT') || hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Flux<RatingDTO> getAllIdeasRatings(@PathVariable String ideaId){
        return ratingService.getRatings(ideaId);
    }

    @GetMapping("/{ideaId}")
    @PreAuthorize("hasAuthority('EXPERT') || hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<RatingDTO> getExpertRatingForIdea(@PathVariable String ideaId, @AuthenticationPrincipal User user){
        return ratingService.getExpertRating(ideaId, user.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Рейтинг не найден")));
    }

    @PutMapping("/save")
    @PreAuthorize("hasAuthority('EXPERT') || hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> saveRating(@RequestBody RatingDTO ratingDTO, @AuthenticationPrincipal User user) {
        return ratingService.saveRating(ratingDTO, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное сохранение рейтинга"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST, "Рейтинг не сохранен"));
    }

    @PutMapping("/confirm")
    @PreAuthorize("hasAuthority('EXPERT') || hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> confirmRating(@RequestBody RatingDTO ratingDTO, @AuthenticationPrincipal User user) {
        return ratingService.confirmRating(ratingDTO, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное утверждение рейтинга"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Рейтинг не утвержден"));
    }
}
