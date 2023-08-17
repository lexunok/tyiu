package com.tyiu.corn.service;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Idea;
import com.tyiu.corn.repository.CommentRepository;
import com.tyiu.corn.repository.IdeaRepository;
import lombok.RequiredArgsConstructor;

import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CommentService {
    private final CommentRepository commentRepository;
    private final IdeaRepository ideaRepository;

    @Cacheable(cacheNames = {"saveCommentCache"}, key = "comment")
    public Comment saveComment(Comment comment, Long ideaId) {
        Idea idea = ideaRepository.findById(ideaId).orElseThrow();
        comment.setIdea(idea);
        return commentRepository.save(comment);
    }
    @Cacheable(cacheNames = {"saveCommentCache"}, key = "comment")
    public void deleteComment(Long commentId) {
        commentRepository.deleteById(commentId);
    }
}
