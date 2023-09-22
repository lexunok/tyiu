package com.tyiu.corn.service;

import java.time.Instant;
import java.util.List;
import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.repository.CommentRepository;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;


@Service
@RequiredArgsConstructor
public class CommentService {

    private final CommentRepository commentRepository;
    private final Sinks.Many<Comment> sink = Sinks.many().multicast().onBackpressureBuffer();
    private final ModelMapper mapper;

    public Flux<CommentDTO> getAllComments(String ideaId) {
        return commentRepository.findByIdeaId(ideaId)
                .flatMap(c -> Flux.just(mapper.map(c,CommentDTO.class)));
    }

    public Flux<CommentDTO> getNewComments(String ideaId) {
        return sink.asFlux().flatMap(c -> {
            if (c.getIdeaId().equals(ideaId)){
                return Flux.just(mapper.map(c, CommentDTO.class));
            }
            return Flux.empty();
        });
    }

    public Mono<Void> createComment(String ideaId,CommentDTO commentDTO, String email) {
        Comment comment = Comment.builder()
                .ideaId(ideaId)
                .comment(commentDTO.getComment())
                .createdAt(Instant.now())
                .checkedBy(List.of(email))
                .sender(email)
                .build();
        commentRepository.save(comment).subscribe(sink::tryEmitNext);
        return Mono.empty();
    }


    public Mono<Void> deleteComment(String commentId) {
        commentRepository.deleteById(commentId).subscribe();
        sink.asFlux().filter(c -> !c.getId().equals(commentId)).subscribe();
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