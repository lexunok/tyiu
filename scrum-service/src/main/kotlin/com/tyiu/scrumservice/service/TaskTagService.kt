package com.tyiu.scrumservice.service

import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.map
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import java.math.BigInteger

@Service
class TaskTagService (
    private val taskTagRepository: TaskTagRepository,
    val template: R2dbcEntityTemplate
) {

    private val connections:MutableMap<String, MutableSharedFlow<TaskTag>> = mutableMapOf()

    fun getAllTags(): Flow<TaskTagDTO> =
        taskTagRepository.findAll().map { tag ->
            return@map tag.toDTO()
        }

    suspend fun createTag(taskTagDTO:TaskTagDTO):TaskTagDTO {
        val tag = TaskTag(
            name = taskTagDTO.name,
            color = taskTagDTO.color,
            isConfirmed = taskTagDTO.isConfirmed,
        )
        val tagToDTO = taskTagRepository.save(tag).toDTO()
        return tagToDTO
    }

    suspend fun updateTag(tagId: BigInteger, taskTagDTO: TaskTagDTO) {
        val query = "UPDATE task_tag SET name = :name, color = :color, isConfirmed = :isConfirmed WHERE id = :tagId"
        return template.databaseClient.sql(query)
            .bind("name",taskTagDTO.name!!)
            .bind("color",taskTagDTO.color!!)
            .bind("isConfirmed",taskTagDTO.isConfirmed!!)
            .bind("tagId", tagId).await()
    }

    suspend fun deleteTag(tagId: BigInteger): Flow<TaskTag> = taskTagRepository.deleteTagById(tagId)
}