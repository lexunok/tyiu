package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.CommentDTO;
import com.tyiu.corn.service.CommentService;
import lombok.RequiredArgsConstructor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.Payload;
import org.springframework.web.bind.annotation.*;

import java.security.Principal;
import java.util.List;
import java.util.Map;

import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/comment")
@Slf4j
public class CommentController {

    private final CommentService commentService;

    @GetMapping("/all/{id}")
    public  Flux<CommentDTO> getAllIdeaComments(@PathVariable String id){
        return commentService.getAllIdeaComments(id);
    }
    @MessageMapping("comment.{id}.receive")
    public  Flux<CommentDTO> receiveComments(@DestinationVariable String id, @Payload CommentDTO commentDTO){
        System.out.println(commentDTO.getComment());
        return commentService.sendCommentToClients(id);
    }

    @PostMapping("/send/{ideaId}")
    public  Mono<Void> createComment(
            @PathVariable String ideaId, @RequestBody CommentDTO comment, Principal principal){
        return commentService.createComment(ideaId,comment, principal.getName());
    }
    @DeleteMapping("/delete/{ideaId}/{commentId}")
    public  Mono<Void> deleteComment(
            @PathVariable String ideaId, @PathVariable String commentId){
        return commentService.deleteComment(commentId, ideaId);
    }

    @PutMapping("/check/{commentId}")
    public Mono<Void> checkComment(@PathVariable String commentId, Principal principal){
        commentService.checkCommentByUser(commentId, principal.getName());
        return Mono.empty();
    }
}