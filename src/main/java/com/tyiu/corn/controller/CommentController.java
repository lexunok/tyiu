package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.service.CommentService;
import lombok.RequiredArgsConstructor;

import java.util.Map;

import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/comment")
@RequiredArgsConstructor
public class CommentController {
    private final CommentService commentService;

    @PostMapping("/add/{ideaId}")
    public Comment saveComment(@RequestBody Comment comment, @PathVariable Long ideaId) {
        return commentService.createComment(ideaId, comment);
    }
    @DeleteMapping("/delete/{commentId}")
    public Map<String, String> deleteComment(@PathVariable Long commentId) {
        return Map.of("success", "Успешное удаление комментария");
    }
    @PutMapping("/check")
    public void checkComment(Comment comment){
        commentService.checkCommentByUser(comment);
    }
}
