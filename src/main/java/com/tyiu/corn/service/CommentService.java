package com.tyiu.corn.service;

import java.util.Date;

import com.tyiu.corn.exception.AccessException;
import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
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

    @Transactional
    public Comment createComment(Long ideaId, Comment comment) {
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new NotFoundException(String.format("Идеи с id %d не существует", ideaId)));
        comment.setIdea(idea);
        comment.setDateCreated(new Date());
        Comment savedComment = commentRepository.save(comment);
        idea.getComments().add(savedComment);
        ideaRepository.save(idea);
        return comment;
    }

    public void deleteComment(Long ideaId, Long commentId, String email) {
        if (commentRepository.findById(commentId).get().getSender().equals(email)){
            ideaRepository.findById(ideaId).get().getComments().removeIf(comment -> comment.getId() == commentId);
            commentRepository.deleteById(commentId);
        }
        else {
            throw new AccessException("Доступ запрещен");
        }
    }

    @Transactional
    public void checkCommentByUser(Comment comment, Long ideaId) {
        Idea idea = ideaRepository.findById(ideaId).get();
        Comment currentComment = idea.getComments().stream()
        .filter((ideaComment) -> ideaComment.getId() != comment.getId()).findFirst().get();

        idea.getComments().removeIf(ideaComment -> ideaComment.getId() == comment.getId());
        currentComment.setCheckedBy(comment.getCheckedBy());
        commentRepository.save(currentComment);
        idea.getComments().add(currentComment);
        ideaRepository.save(idea);
    }
}