package com.tyiu.corn.repository;

import com.tyiu.corn.model.entities.Comment;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;

import java.util.List;


@Repository
public interface CommentRepository extends ReactiveCrudRepository<Comment, String> {
    Flux<Comment> findAllByIdea_Id(String ideaId);
}
