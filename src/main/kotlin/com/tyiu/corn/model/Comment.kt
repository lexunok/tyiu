package com.tyiu.corn.model

import com.tyiu.corn.model.dto.UserDTO
import com.tyiu.corn.model.entities.User
import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDateTime

interface CommentRepository: CoroutineCrudRepository<Comment, String> {
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
