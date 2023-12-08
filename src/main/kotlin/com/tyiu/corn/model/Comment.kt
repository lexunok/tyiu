package com.tyiu.corn.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDateTime

interface CommentRepository: CoroutineCrudRepository<Comment, String> {
    fun findAllByIdeaId(ideaId: String): Flow<Comment>
}

@Table
data class Comment (
        @Id
        val id: String? = null,
        val text: String? = null,
        val senderEmail: String? = null,
        val checkedBy: List<String>? = null,
        val createdAt: LocalDateTime? = LocalDateTime.now(),
        val ideaId: String? = null
)

data class CommentDTO (
        val id: String? = null,
        val text: String? = null,
        val senderEmail: String? = null,
        val checkedBy: List<String>? = null,
        val createdAt: LocalDateTime? = LocalDateTime.now(),
        val ideaId: String? = null,
)

fun Comment.toDTO(): CommentDTO = CommentDTO(
        id = id,
        ideaId = ideaId,
        text = text,
        checkedBy = checkedBy,
        createdAt = createdAt,
        senderEmail = senderEmail
)
