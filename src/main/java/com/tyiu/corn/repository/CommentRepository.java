package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Comment;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

import java.util.List;



public interface CommentRepository extends ReactiveCrudRepository<Comment, Long> {
    Flux<Comment> findAllByIdea_Id(Long ideaId);
}
