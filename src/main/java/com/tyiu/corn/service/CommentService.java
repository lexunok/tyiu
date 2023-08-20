package com.tyiu.corn.service;

import java.util.Date;
import java.util.List;

import com.tyiu.corn.exception.AccessException;
import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.repository.CommentRepository;
import com.tyiu.corn.repository.IdeaRepository;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CommentService {
    private final CommentRepository commentRepository;
    private final IdeaRepository ideaRepository;

    public List<Comment> getAllIdeaComments(Long ideaId){
        if (ideaRepository.existsById(ideaId)){
            return commentRepository.findAllByIdea_Id(ideaId);
        }
        throw new NotFoundException(String.format("Идеи с id %d не существует", ideaId));
        
    }

    public Comment createComment(Long ideaId, Comment comment, String email) {
        comment.setIdea(ideaRepository.findById(ideaId).orElseThrow(
            () -> new NotFoundException(String.format("Идеи с id %d не существует", ideaId)))
        );
        comment.setIdeaId(ideaId);
        comment.setDateCreated(new Date());
        comment.setSender(email);
        comment.setCheckedBy(List.of(email));
        return commentRepository.save(comment);
    }

    @Transactional
    public void deleteComment(Long commentId, String email) {
        if (commentRepository.findById(commentId).get().getSender().equals(email)){
            commentRepository.deleteById(commentId);
        }
        else {
            throw new AccessException("Доступ запрещен");
        }
    }

    @Transactional
    public void checkCommentByUser(Long commentId, String email) {
        Comment currentComment = commentRepository.findById(commentId).get();
        currentComment.getCheckedBy().add(email);
        commentRepository.save(currentComment);
    }
}