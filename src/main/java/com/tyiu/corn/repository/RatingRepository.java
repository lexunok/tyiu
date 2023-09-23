package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Rating;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Repository
public interface RatingRepository extends ReactiveCrudRepository<Rating, String> {
    Flux<Rating> findAllByIdeaId(String ideaId);

    Mono<Rating> findFirstByExpertAndIdeaId(String expert, String ideaId);
}