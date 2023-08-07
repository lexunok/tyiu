package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.service.CommentService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/comment")
@RequiredArgsConstructor
public class CommentContoller {
    private final CommentService commentService;

    @PostMapping("{ideaId}/add")
    public Comment saveComment(@RequestBody Comment comment, @PathVariable Long ideaId) {
        return commentService.saveComment(comment, ideaId);
    }

    @DeleteMapping("/delete/{commentID}")
    public void deleteComment(@PathVariable Long commentID) {
        commentService.deleteComment(commentID);
    }
}
