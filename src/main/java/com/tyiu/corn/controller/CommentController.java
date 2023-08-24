package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.service.CommentService;
import lombok.RequiredArgsConstructor;

import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.web.bind.annotation.*;

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
    public Map<String, List<CommentDTO>> getAllIdeaComments(@PathVariable Long ideaId){
        return Map.of("comments", commentService.getAllIdeaComments(ideaId));
    }

    @MessageMapping("/comments/create-comment/{ideaId}")
    @SendTo("/topic/comments/{ideaId}")
    public CommentDTO createComment(@DestinationVariable Long ideaId, @Payload CommentDTO commentDTO, Principal principal) {
        return commentService.createComment(ideaId, commentDTO, principal.getName());
    }

    @MessageMapping("/comments/delete-comment/{commentId}")
    @SendTo("/topic/comments/{ideaId}")
    public Map<String, Long> deleteComment(@DestinationVariable Long commentId, Principal principal) {
        commentService.deleteComment(commentId, principal.getName());
        return Map.of("commentId", commentId);
    }

    @PostMapping("/add/{ideaId}")
    public CommentDTO saveCommentOutdated(@RequestBody CommentDTO commentDTO, @PathVariable Long ideaId, Principal principal) {
        return commentService.createComment(ideaId, commentDTO, principal.getName());
    }
    @DeleteMapping("/delete/{commentId}")
    public Map<String, String> deleteCommentOutdated(@PathVariable Long commentId, Principal principal) {
        commentService.deleteComment(commentId, principal.getName());
        return Map.of("success", "Успешное удаление комментария");
    }
    @PutMapping("/check/{commentId}")
    public void checkComment(@PathVariable Long commentId, Principal principal){
        commentService.checkCommentByUser(commentId, principal.getName());
    }
}