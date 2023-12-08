package com.tyiu.corn.service

import com.tyiu.corn.model.entities.Comment
import com.tyiu.corn.model.entities.CommentDTO
import com.tyiu.corn.model.entities.CommentRepository
import com.tyiu.corn.model.entities.toDTO
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.map
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service

@Service
class CommentService(private val repository: CommentRepository, val template: R2dbcEntityTemplate) {

    private val connections:MutableMap<String, MutableSharedFlow<Comment>> = mutableMapOf()

    fun getNewComments(ideaId: String): Flow<CommentDTO> =
            connections.getOrDefault(ideaId, MutableSharedFlow()).map { c -> c.toDTO() }

    fun getAllComments(ideaId: String): Flow<CommentDTO> =
            repository.findAllByIdeaId(ideaId).map { c -> c.toDTO()}

    suspend fun createComment(commentDTO: CommentDTO, senderEmail: String): CommentDTO {
        val comment = Comment(
                ideaId = commentDTO.ideaId,
                text = commentDTO.text,
                senderEmail = senderEmail,
                checkedBy = listOf(senderEmail),
        )
        connections.getOrPut(comment.ideaId?:throw Error()){MutableSharedFlow()}.tryEmit(comment)
        return repository.save(comment).toDTO()
    }

    suspend fun deleteComment(commentId: String) = repository.deleteById(commentId)

    suspend fun checkCommentByUser(commentId: String, userEmail: String) {
        val query = "UPDATE comment SET checked_by = array_append(checked_by,:userEmail) WHERE id =:commentId"
        return template.databaseClient.sql(query)
                .bind("userEmail", userEmail)
                .bind("commentId", commentId).await()
    }
}