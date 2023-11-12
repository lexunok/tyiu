package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.service.CommentService;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.web.bind.annotation.*;
import java.security.Principal;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/comment")
public class CommentController {

    private final CommentService commentService;

    @MessageMapping("comment.{ideaId}.receive")
    public Flux<CommentDTO> receiveNewComments(@DestinationVariable String ideaId){
        return commentService.getNewComments(ideaId);
    }

    @GetMapping("/all/{ideaId}")
    public Flux<CommentDTO> getAllComments(@PathVariable String ideaId){
        return commentService.getAllComments(ideaId);
    }

    @PostMapping("/send")
    public Mono<CommentDTO> createComment(@RequestBody CommentDTO comment, Principal principal){
        return commentService.createComment(comment, principal.getName());
    }

    @DeleteMapping("/delete/{commentId}")
    public Mono<Void> deleteComment(@PathVariable String commentId){
        return commentService.deleteComment(commentId);
    }
    @PutMapping("/check/{commentId}")
    public Mono<Void> checkComment(@PathVariable String commentId, Principal principal){
        return commentService.checkCommentByUser(commentId, principal.getName());
    }

}