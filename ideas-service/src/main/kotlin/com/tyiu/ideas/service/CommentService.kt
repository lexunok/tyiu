package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.reactive.asFlow
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import reactor.core.publisher.Sinks
import java.util.concurrent.ConcurrentHashMap


@Service
class CommentService(private val commentRepository: CommentRepository,
                     private val userRepository: UserRepository, val template: R2dbcEntityTemplate) {

    private val connections = ConcurrentHashMap<String, Sinks.Many<Comment>>();

    private fun getOrCreateConnection(ideaId: String): Sinks.Many<Comment> {
        return connections.computeIfAbsent(ideaId) {
            Sinks.many().multicast().onBackpressureBuffer()
        }
    }

    fun getNewComments(ideaId: String): Flow<CommentDTO> =
            getOrCreateConnection(ideaId).asFlux().asFlow().map { c ->
                val comment = c.toDTO()
                comment.sender = c.senderId?.let { userRepository.findById(it) }?.toDTO()
                return@map comment
            }

    fun getAllComments(ideaId: String): Flow<CommentDTO> =
        commentRepository.findAllByIdeaId(ideaId).map { c ->
            val comment = c.toDTO()
            comment.sender = c.senderId?.let { userRepository.findById(it) }?.toDTO()
            return@map comment
        }

    suspend fun createComment(commentDTO: CommentDTO, senderId: String): CommentDTO {
        val comment = Comment(
                ideaId = commentDTO.ideaId,
                text = commentDTO.text,
                senderId = senderId,
                checkedBy = listOf(senderId),
        )

        val commentToDTO = commentRepository.save(comment).toDTO()
        commentDTO.ideaId?.let { getOrCreateConnection(ideaId = it) }?.tryEmitNext(comment)
        commentToDTO.sender = comment.senderId?.let { userRepository.findById(it) }?.toDTO()
        return commentToDTO
    }

    suspend fun deleteComment(commentId: String) = commentRepository.deleteById(commentId)

    suspend fun checkCommentByUser(commentId: String, userId: String) {
        val query = "UPDATE comment SET checked_by = array_append(checked_by,:userId) WHERE id =:commentId"
        template.databaseClient.sql(query)
                .bind("userId", userId)
                .bind("commentId", commentId).await()
    }
}