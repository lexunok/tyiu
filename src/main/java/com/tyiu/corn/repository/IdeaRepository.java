package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.StatusIdea;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
@Repository
public interface IdeaRepository extends ReactiveCrudRepository<Idea, String> {
}