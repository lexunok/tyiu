package com.tyiu.corn.service;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.repository.CommentRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;


@Service
@RequiredArgsConstructor
@Slf4j
public class CommentService {

    private final CommentRepository commentRepository;
    private final Map<String, Sinks.Many<CommentDTO>> userSinks = new ConcurrentHashMap<>();
    private final ModelMapper mapper;

    public Flux<CommentDTO> getAllComments(String ideaId) {
        return commentRepository.findByIdeaId(ideaId)
                .flatMap(c -> Flux.just(mapper.map(c,CommentDTO.class)));
    }

    public Flux<CommentDTO> getNewComments(String ideaId) {
        Sinks.Many<CommentDTO> sink = userSinks
                .computeIfAbsent(ideaId, key -> Sinks.many().multicast().onBackpressureBuffer());
        return sink.asFlux().doOnCancel(() -> userSinks.remove(ideaId));
    }

    public Mono<Void> createComment(String ideaId,CommentDTO commentDTO, String email) {
        Comment comment = Comment.builder()
                .ideaId(ideaId)
                .comment(commentDTO.getComment())
                .createdAt(Instant.now())
                .checkedBy(List.of(email))
                .sender(email)
                .build();
        Sinks.Many<CommentDTO> sink = userSinks.get(ideaId);
        return commentRepository.save(comment).doOnSuccess(c -> {
            if (sink != null) {
                sink.tryEmitNext(mapper.map(c, CommentDTO.class));
            }}).then();
    }


    public Mono<Void> deleteComment(String commentId) {
        commentRepository.deleteById(commentId).subscribe();
        return Mono.empty();
    }

    public Mono<Void> checkCommentByUser(String commentId, String email) {
        Mono<Comment> currentComment = commentRepository.findById(commentId);
        currentComment.flatMap(c -> {
            c.getCheckedBy().add(email);
            return commentRepository.save(c);
        });
        return Mono.empty();
    }

}