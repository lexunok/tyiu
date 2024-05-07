package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.UserDTO
import com.tyiu.ideas.model.entities.Idea
import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.publisher.NotificationPublisher
import enums.PortalLinks
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.reactive.asFlow
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Query.query
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import request.NotificationRequest
import reactor.core.scheduler.Schedulers


@Service
class CommentService(private val commentRepository: CommentRepository,
                     private val userRepository: UserRepository,
                     private val notificationPublisher: NotificationPublisher,
                     val template: R2dbcEntityTemplate) {

    private suspend fun sendNotification(senderId: String, ideaId: String?){
        val idea = template.selectOne(query(where("id").`is`(ideaId!!)), Idea::class.java).awaitSingle()
        val sender = template.selectOne(query(where("id").`is`(senderId)), User::class.java).awaitSingle()
        if (senderId != idea.initiatorId){
            notificationPublisher.makeNotification(
                NotificationRequest(
                    null,
                    template.selectOne(query(where("id").`is`(idea.initiatorId)), User::class.java).awaitSingle().email,
                    null,
                    "Новый комментарий от эксперта в идеи",
                    "Эксперт ${sender.firstName} ${sender.lastName} оставил комментарий в идеи ${idea.name}",
                    PortalLinks.IDEAS_LIST.name + ideaId,
                    "Перейти к идеи",
                    sender.email
                )
            ).publishOn(Schedulers.boundedElastic()).awaitSingle()
        }
        else {
            val query = """
                SELECT
                    e.email
                FROM group g
                    LEFT JOIN group_user ON gu.group_id = g.id
                    LEFT JOIN users e ON e.id = gu.user_id
                WHERE g.id = :groupId
            """.trimIndent()
            template.databaseClient
                .sql(query)
                .bind("groupId", idea.groupExpertId)
                .map { row, _ -> UserDTO(
                    null,
                    row.get("email", String::class.java),
                    null,
                    null
                ) }
                .all()
                .asFlow()
                .collect {
                    notificationPublisher.makeNotification(
                        NotificationRequest(
                            null,
                            it.email,
                            null,
                            "Новый комментарий инициатора в идеи",
                            "Инициатор ${sender.firstName} ${sender.lastName} оставил комментарий в идеи ${idea.name}",
                            PortalLinks.IDEAS_LIST.name + ideaId,
                            "Перейти к идеи",
                            sender.email
                        )
                    ).publishOn(Schedulers.boundedElastic()).awaitSingle()
                }
        }
    }


    private val connections:MutableMap<String, MutableSharedFlow<Comment>> = mutableMapOf()
    fun getNewComments(ideaId: String): Flow<CommentDTO> =
            connections.getOrDefault(ideaId, MutableSharedFlow()).map { c ->
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
        connections.getOrPut(comment.ideaId?:throw Error()){MutableSharedFlow()}.tryEmit(comment)
        commentToDTO.sender = comment.senderId?.let { userRepository.findById(it) }?.toDTO()
        sendNotification(senderId, commentDTO.ideaId)
        return commentToDTO
    }

    suspend fun deleteComment(commentId: String) = commentRepository.deleteById(commentId)

    suspend fun checkCommentByUser(commentId: String, userId: String) {
        val query = "UPDATE comment SET checked_by = array_append(checked_by,:userId) WHERE id =:commentId"
        return template.databaseClient.sql(query)
                .bind("userId", userId)
                .bind("commentId", commentId).await()
    }
}