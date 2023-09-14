package com.tyiu.corn.service;

import java.net.URI;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.repository.CommentRepository;
import com.tyiu.corn.repository.IdeaRepository;
import io.rsocket.core.RSocketClient;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.codec.cbor.Jackson2CborDecoder;
import org.springframework.http.codec.cbor.Jackson2CborEncoder;
import org.springframework.messaging.rsocket.RSocketRequester;
import org.springframework.messaging.rsocket.RSocketStrategies;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
public class CommentService {

    private final CommentRepository commentRepository;
    private final ModelMapper mapper;

    public Flux<CommentDTO> getAllComments(String ideaId) {
        Flux<Comment> ideaComments = commentRepository.findWithTailableCursorByIdeaId(ideaId);
        return ideaComments.flatMap(c -> Flux.just(mapper.map(c,CommentDTO.class)));
    }

    public Mono<Void> createComment(String ideaId,CommentDTO commentDTO, String email) {
        Comment comment = Comment.builder()
                .ideaId(ideaId)
                .comment(commentDTO.getComment())
                .createdAt(Instant.now())
                .checkedBy(List.of(email))
                .sender(email)
                .build();
        commentRepository.save(comment).subscribe();
        return Mono.empty();
    }


    public Mono<Void> deleteComment(String commentId, String ideaId) {
        commentRepository.deleteById(commentId).subscribe();
        return Mono.empty();
    }

    public void checkCommentByUser(String commentId, String email) {
        Mono<Comment> currentComment = commentRepository.findById(commentId);
        currentComment.flatMap(c -> {
            c.getCheckedBy().add(email);
            return commentRepository.save(c);
        });
    }

}