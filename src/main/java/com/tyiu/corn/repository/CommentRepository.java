package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Comment;
import org.springframework.data.mongodb.repository.Tailable;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;


@Repository
public interface CommentRepository extends ReactiveCrudRepository<Comment, String> {
    @Tailable
    Flux<Comment> findWithTailableCursorByIdeaId(String ideaId);
}

