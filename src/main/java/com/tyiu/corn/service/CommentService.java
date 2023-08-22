package com.tyiu.corn.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.tyiu.corn.exception.AccessException;
import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.repository.CommentRepository;
import com.tyiu.corn.repository.IdeaRepository;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = {"comments"})
@Slf4j
public class CommentService {
    private final CommentRepository commentRepository;
    private final IdeaRepository ideaRepository;

    private final ModelMapper mapper;

    private boolean containsEmail(List<String> list, String email){
        return list.stream().filter(listEmail -> listEmail == email).findFirst().isPresent();
    }

    @Cacheable
    public List<CommentDTO> getAllIdeaComments(Long ideaId){
        if (ideaRepository.existsById(ideaId)){
            List<Comment> ideaComments = commentRepository.findAllByIdea_Id(ideaId);
            return mapper.map(ideaComments, new TypeToken<List<CommentDTO>>(){}.getType());
        }
        throw new NotFoundException(String.format("Идеи с id %d не существует", ideaId)); 
    }

    @CacheEvict(allEntries = true)
    public CommentDTO createComment(Long ideaId, CommentDTO commentDTO, String email) {
        commentDTO.setIdeaId(ideaId);
        commentDTO.setDateCreated(new Date());
        commentDTO.setSender(email);
        commentDTO.setCheckedBy(List.of(email));
        Comment comment = mapper.map(commentDTO, Comment.class);
        comment.setIdea(ideaRepository.findById(ideaId).orElseThrow(
            () -> new NotFoundException(String.format("Идеи с id %d не существует", ideaId)))
        );
        comment = commentRepository.save(comment);
        return mapper.map(comment, CommentDTO.class);
    }

    @CacheEvict(allEntries = true)
    @Transactional
    public void deleteComment(Long commentId, String email) {
        if (commentRepository.findById(commentId).get().getSender().equals(email)){
            commentRepository.deleteById(commentId);
        }
        else {
            throw new AccessException("Доступ запрещен");
        }
    }

    @CacheEvict(allEntries = true)
    public void checkCommentByUser(Long commentId, String email) {
        Comment currentComment = commentRepository.findById(commentId).orElseThrow(            
            () -> new NotFoundException("Комментария не существует")
        );
        currentComment.getCheckedBy().add(email);
        commentRepository.save(currentComment);
    }
}