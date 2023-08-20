package com.tyiu.corn.controller;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.service.CommentService;
import lombok.RequiredArgsConstructor;

import java.security.Principal;
import java.util.List;
import java.util.Map;

import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/comment")
@RequiredArgsConstructor
public class CommentController {
    private final CommentService commentService;

    @GetMapping("/get-idea-comments/{ideaId}")
    public Map<String, List<Comment>> getAllIdeaComments(@PathVariable Long ideaId){
        return Map.of("comments", commentService.getAllIdeaComments(ideaId));
    }

    @PostMapping("/add/{ideaId}")
    public Comment saveComment(@RequestBody Comment comment, @PathVariable Long ideaId, Principal principal) {
        return commentService.createComment(ideaId, comment, principal.getName());
    }
    @DeleteMapping("/delete/{commentId}")
    public Map<String, String> deleteComment(@PathVariable Long commentId, Principal principal) {
        commentService.deleteComment(commentId, principal.getName());
        return Map.of("success", "Успешное удаление комментария");
    }
    @PutMapping("/check/{commentId}")
    public void checkComment(@PathVariable Long commentId, Principal principal){
        commentService.checkCommentByUser(commentId, principal.getName());
    }
}