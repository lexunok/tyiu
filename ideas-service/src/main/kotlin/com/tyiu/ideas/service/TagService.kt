package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service

@Service
class TagService (private val taskTagRepository: TagRepository, val template: R2dbcEntityTemplate) {

    private fun buildTag(confirm: Boolean, tagDTO: TagDTO, userId: String): Tag{
        return Tag(
            name = tagDTO.name,
            color = tagDTO.color,
            confirmed = confirm,
            creatorId = userId
        )
    }

    fun getAllTags(): Flow<TagDTO> = taskTagRepository.findAll().map { it.toDTO() }

    suspend fun createTag(tagDTO: TagDTO, userId: String): TagDTO {
        return taskTagRepository.save(buildTag(true, tagDTO, userId)).toDTO()
    }

    suspend fun createNoConfirmedTag(tagDTO: TagDTO, userId: String): TagDTO {
        return taskTagRepository.save(buildTag(false, tagDTO, userId)).toDTO()
    }

    suspend fun confirmTag(userId: String, tagId: String) {
        val query = "UPDATE tag SET confirmed = true, updater_id = :userId WHERE id = :tagId"
        return template.databaseClient.sql(query)
            .bind("userId", userId)
            .bind("tagId", tagId).await()
    }

    suspend fun updateTag(tagDTO: TagDTO, userId: String, tagId: String) {
        val query = "UPDATE tag SET name = :name, color = :color, updater_id = :userId WHERE id = :tagId"
        return template.databaseClient.sql(query)
            .bind("name", tagDTO.name!!)
            .bind("color", tagDTO.color!!)
            .bind("userId", userId)
            .bind("tagId", tagId).await()
    }

    suspend fun deleteTag(tagId: String) = taskTagRepository.deleteById(tagId)
}