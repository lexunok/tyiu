package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.enums.StatusIdea;

import java.util.List;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
@Repository
public interface IdeaRepository extends ReactiveCrudRepository<Idea, String> {
    Flux<Idea> findAllByStatus (StatusIdea status);
    Flux<Idea> findAllByInitiator (String initiator);
}