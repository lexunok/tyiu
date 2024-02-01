package com.tyiu.scrumservice.service

import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service
import java.math.BigInteger

@Service
class TaskTagService (private val taskTagRepository: TaskTagRepository) {
    fun getAllTags(): Flow<TaskTagDTO> =
        taskTagRepository.findAll().map { tag ->
            return@map tag.toDTO()
        }

    fun postTag():Flow<TaskTag> = taskTagRepository.findAll()

    fun updateTag():Flow<TaskTag> = taskTagRepository.findAll()

    suspend fun deleteTag(tagId: BigInteger): Flow<TaskTag> = taskTagRepository.deleteTagById(tagId)
}