package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.service.CommentService;
import lombok.RequiredArgsConstructor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.web.bind.annotation.*;
import java.security.Principal;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/comment")
@Slf4j
public class CommentController {

    private final CommentService commentService;

    @MessageMapping("comment.{id}.receive")
    public Flux<CommentDTO> receiveNewComments(@DestinationVariable String id){
        return commentService.getNewComments(id).log();
    }

    @GetMapping("/all/{id}")
    public Flux<CommentDTO> getAllComments(@PathVariable String id){
        return commentService.getAllComments(id);
    }

    @PostMapping("/send/{ideaId}")
    public Mono<Void> createComment(
            @PathVariable String ideaId, @RequestBody CommentDTO comment, Principal principal){
        return commentService.createComment(ideaId,comment, principal.getName()).log();
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