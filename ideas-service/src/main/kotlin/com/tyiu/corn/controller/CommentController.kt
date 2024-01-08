package com.tyiu.corn.controller

import com.tyiu.corn.model.CommentDTO
import com.tyiu.corn.model.entities.User
import com.tyiu.corn.service.CommentService
import kotlinx.coroutines.flow.Flow
import org.springframework.messaging.handler.annotation.DestinationVariable
import org.springframework.messaging.handler.annotation.MessageMapping
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/comment")
class CommentController (private val commentService: CommentService) {

    @MessageMapping("comment.{ideaId}.receive")
    fun receiveNewComments(@DestinationVariable ideaId: String): Flow<CommentDTO> = commentService.getNewComments(ideaId)

    @GetMapping("/all/{ideaId}")
    fun getAllComments(@PathVariable ideaId: String): Flow<CommentDTO> = commentService.getAllComments(ideaId)

    @PostMapping("/send")
    suspend fun createComment(@RequestBody comment: CommentDTO, @AuthenticationPrincipal user: User): CommentDTO =
            commentService.createComment(comment, user.id)


    @DeleteMapping("/delete/{commentId}")
    suspend fun deleteComment(@PathVariable commentId: String) = commentService.deleteComment(commentId)

    @PutMapping("/check/{commentId}")
    suspend fun checkComment(@PathVariable commentId: String, @AuthenticationPrincipal user: User) =
            commentService.checkCommentByUser(commentId, user.id)
}