package com.tyiu.corn.service;

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
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = {"comments"})
@Slf4j
public class CommentService {
    private final CommentRepository commentRepository;
    private final IdeaRepository ideaRepository;


//    @Cacheable
    public Flux<CommentDTO> getAllIdeaComments(String ideaId) {
        Flux<Comment> ideaComments = commentRepository.findAllByIdea_Id(ideaId);
        return ideaComments.cast(CommentDTO.class);
    }

//    @CacheEvict(allEntries = true)
    public Mono<CommentDTO> createComment(String ideaId, CommentDTO commentDTO, String email) {
        commentDTO.setIdeaId(ideaId);
        commentDTO.setDateCreated(new Date());
        commentDTO.setSender(email);
        commentDTO.setCheckedBy(List.of(email));
        return Mono.just(commentDTO).cast(Comment.class).flatMap(commentRepository::save).cast(CommentDTO.class);
    }

//    @CacheEvict(allEntries = true)
//    @Transactional
    public void deleteComment(String commentId, String email) {
        commentRepository.deleteById(commentId);
    }

//    @CacheEvict(allEntries = true)
    public void checkCommentByUser(String commentId, String email) {
        Mono<Comment> currentComment = commentRepository.findById(commentId);
        currentComment.flatMap(c -> {
            c.getCheckedBy().add(email);
            return commentRepository.save(c);
        });
    }

}