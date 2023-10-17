package com.tyiu.corn.service;

import java.time.Instant;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.dto.CommentDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;


@Service
@RequiredArgsConstructor
@Slf4j
public class CommentService {

    private final R2dbcEntityTemplate template;
    private final Map<Long, Sinks.Many<CommentDTO>> userSinks = new ConcurrentHashMap<>();
    private final ModelMapper mapper;

    public Flux<CommentDTO> getAllComments(Long ideaId) {
        return template.select(query(where("idea_id").is(ideaId)), Comment.class)
                .thenMany(c -> mapper.map(c,CommentDTO.class));
    }

    public Flux<CommentDTO> getNewComments(Long ideaId) {
        Sinks.Many<CommentDTO> sink = userSinks
                .computeIfAbsent(ideaId, key -> Sinks.many().multicast().onBackpressureBuffer());
        return sink.asFlux().doOnCancel(() -> userSinks.remove(ideaId));
    }

    public Mono<Void> createComment(CommentDTO commentDTO, String email) {
        Comment comment = Comment.builder()
                .ideaId(commentDTO.getIdeaId())
                .text(commentDTO.getText())
                .createdAt(LocalDateTime.now())
                .senderEmail(email)
                .build();
        Sinks.Many<CommentDTO> sink = userSinks.get(commentDTO.getIdeaId());
        return template.insert(comment).doOnSuccess(c -> {
            if (sink!=null){
                sink.tryEmitNext(mapper.map(c,CommentDTO.class));
            }
        }).then();
    }


    public Mono<Void> deleteComment(String commentId) {
        return template.delete(query(where("id").is(commentId)), Comment.class).then();
    }

}