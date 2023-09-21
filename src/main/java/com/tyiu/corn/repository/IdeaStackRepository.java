package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.IdeaStack;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;

import reactor.core.publisher.Flux;

@Repository
public interface IdeaStackRepository extends ReactiveCrudRepository<IdeaStack, String> {
    Flux<IdeaStack> deleteByIdeaId(String ideaId);
}
