package com.tyiu.ideas.controller

import com.tyiu.ideas.model.CommentDTO
import com.tyiu.ideas.service.CommentService
import kotlinx.coroutines.flow.Flow
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/ideas-service/comment")
class CommentController (private val commentService: CommentService) {

    @GetMapping("/all/{ideaId}")
    fun getAllComments(@PathVariable ideaId: String): Flow<CommentDTO> = commentService.getAllComments(ideaId)

    @PostMapping("/send")
    suspend fun createComment(@RequestBody comment: CommentDTO, @AuthenticationPrincipal jwt: Jwt): CommentDTO =
            commentService.createComment(comment, jwt.id)


    @DeleteMapping("/delete/{commentId}")
    suspend fun deleteComment(@PathVariable commentId: String) = commentService.deleteComment(commentId)

    @PutMapping("/check/{commentId}")
    suspend fun checkComment(@PathVariable commentId: String, @AuthenticationPrincipal jwt: Jwt) =
            commentService.checkCommentByUser(commentId, jwt.id)
}