package com.tyiu.ideas.model

import com.tyiu.client.models.UserDTO
import com.tyiu.ideas.model.entities.User
import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDateTime

interface CommentRepository: CoroutineCrudRepository<Comment, String> {

        @Query("SELECT * FROM comment WHERE idea_id = :ideaId ORDER BY created_at ASC")
        fun findAllByIdeaId(ideaId: String): Flow<Comment>
}
interface UserRepository: CoroutineCrudRepository<User, String> {}

@Table
data class Comment (
        @Id
        val id: String? = null,
        val text: String? = null,
        val senderId: String? = null,
        val checkedBy: List<String>? = null,
        val createdAt: LocalDateTime? = LocalDateTime.now(),
        val ideaId: String? = null
)

data class CommentDTO (
        val id: String? = null,
        val text: String? = null,
        var sender: UserDTO? = null,
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
)

fun User.toDTO(): UserDTO = UserDTO(
        id,
        email,
        firstName,
        lastName,
)
