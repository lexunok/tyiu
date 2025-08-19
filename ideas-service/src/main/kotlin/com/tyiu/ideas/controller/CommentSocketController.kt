package com.tyiu.ideas.controller

import com.tyiu.ideas.model.CommentDTO
import com.tyiu.ideas.service.CommentService
import kotlinx.coroutines.flow.Flow
import org.springframework.messaging.handler.annotation.DestinationVariable
import org.springframework.messaging.handler.annotation.MessageMapping
import org.springframework.stereotype.Controller

@Controller
@MessageMapping("api.v1.comment")
class CommentSocketController(private val commentService: CommentService) {
    @MessageMapping("receive.{ideaId}")
    fun receiveNewComments(@DestinationVariable ideaId: String): Flow<CommentDTO> = commentService.getNewComments(ideaId)
}