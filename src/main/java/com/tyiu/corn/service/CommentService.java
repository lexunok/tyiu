package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.repository.CommentRepository;
import com.tyiu.corn.repository.IdeaRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CommentService {
    private final CommentRepository commentRepository;
    private final IdeaRepository ideaRepository;

    public Comment saveComment(Comment comment, Long ideaId) {
        Idea idea = ideaRepository.findById(ideaId).orElseThrow();
        comment.setIdea(idea);
        return commentRepository.save(comment);
    }

    public void deleteComment(Long commentId) {
        commentRepository.deleteById(commentId);
    }
}
