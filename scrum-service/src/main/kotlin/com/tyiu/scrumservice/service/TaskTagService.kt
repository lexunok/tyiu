package com.tyiu.scrumservice.service

import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service

@Service
class TaskTagService (private val taskTagRepository: TaskTagRepository, val template: R2dbcEntityTemplate) {

    fun getAllTags(): Flow<TaskTagDTO> = taskTagRepository.findAll().map { it.toDTO() }

    suspend fun createTag(taskTagDTO: TaskTagDTO): TaskTagDTO {
        val tag = TaskTag(
            name = taskTagDTO.name,
            color = taskTagDTO.color
        )
        return taskTagRepository.save(tag).toDTO()
    }

    suspend fun updateTag(tagId: String, taskTagRequest: TaskTagRequest) {
        val query = "UPDATE task_tag SET name = :tagName, color = :tagColor WHERE id = :tagId"
        return template.databaseClient.sql(query)
            .bind("tagName",taskTagRequest.tagName!!)
            .bind("tagColor",taskTagRequest.tagColor!!)
            .bind("tagId", tagId).await()
    }

    suspend fun deleteTag(tagId: String): Flow<TaskTag> = taskTagRepository.deleteTagById(tagId)
}