package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.service.CommentService;
import lombok.RequiredArgsConstructor;

import org.springframework.web.bind.annotation.*;

import java.security.Principal;
import java.util.List;
import java.util.Map;

import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/comment")
@RequiredArgsConstructor
public class CommentController {
    private final CommentService commentService;

    @GetMapping("/get-idea-comments/{ideaId}")
    public Map<String, Flux<CommentDTO>> getAllIdeaComments(@PathVariable String ideaId){
        return Map.of("comments", commentService.getAllIdeaComments(ideaId));
    }
//

//    @MessageMapping("/comments/create-comment/{ideaId}")
//    @SendTo("/topic/comments/{ideaId}")
//    public Mono<CommentDTO> createComment(@DestinationVariable Long ideaId, @Payload CommentDTO commentDTO, Principal principal) {
//        return commentService.createComment(ideaId, commentDTO, principal.getName());
//    }

//    @MessageMapping("/comments/delete-comment/{commentId}")
//    @SendTo("/topic/comments/{ideaId}")
//    public Map<String, Long> deleteComment(@DestinationVariable Long commentId, Principal principal) {
//        commentService.deleteComment(commentId, principal.getName());
//        return Map.of("commentId", commentId);
//    }

    @PostMapping("/add/{ideaId}")
    public Mono<CommentDTO> saveCommentOutdated(@RequestBody CommentDTO commentDTO, @PathVariable String ideaId, Principal principal) {
        return commentService.createComment(ideaId, commentDTO, principal.getName());
    }
    @DeleteMapping("/delete/{commentId}")
    public Mono<Void> deleteCommentOutdated(@PathVariable String commentId, Principal principal) {
        commentService.deleteComment(commentId, principal.getName());
        return Mono.empty();
    }
    @PutMapping("/check/{commentId}")
    public Mono<Void> checkComment(@PathVariable String commentId, Principal principal){
        commentService.checkCommentByUser(commentId, principal.getName());
        return Mono.empty();
    }
}