package com.tyiu.corn.service;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.repository.CommentRepository;
import com.tyiu.corn.repository.IdeaRepository;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;

import java.util.List;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CommentService {
    private final CommentRepository commentRepository;
    private final IdeaRepository ideaRepository;

    @Transactional
    public Comment createComment(Long ideaId, Comment comment) {
        Idea idea = ideaRepository.findById(ideaId).orElseThrow(() -> new NotFoundException(String.format("Идеи с id %ld не существует", ideaId)));
        comment.setIdea(idea);
        Comment savedComment = commentRepository.save(comment);
        idea.getComments().add(savedComment);
        ideaRepository.save(idea);
        return comment;
    }

    public void deleteComment(Long commentId) {
        commentRepository.deleteById(commentId);
    }

    @Transactional
    public void checkCommentByUser(Comment comment){
        Idea idea = ideaRepository.findById(comment.getIdea().getId()).get();
        idea.getComments().removeIf((ideaComment)-> ideaComment.getId()==comment.getId());
        commentRepository.save(comment);
        idea.getComments().add(comment);
        ideaRepository.save(idea);
    }
}
