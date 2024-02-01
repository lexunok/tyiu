package com.tyiu.scrumservice.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.math.BigInteger


interface TaskTagRepository: CoroutineCrudRepository<TaskTag, String>{
    @Query("DELETE FROM task_tag WHERE id=:tagId")
    fun deleteTagById(tagId: BigInteger): Flow<TaskTag>
}

@Table
data class TaskTag (
    @Id
    val id: String? = null,
    var name: String? = null,
    var color: String? = null,
    var isConfirmed: Boolean? = false
)

data class Task2Tag (
    val taskId: String? = null,
    val tagId: String? = null
)

data class TaskTagDTO (
    val id: String? = null,
    var name: String? = null,
    var color: String? = null,
    var isConfirmed: Boolean? = false
)

fun TaskTag.toDTO(): TaskTagDTO = TaskTagDTO (
    id = id,
    name = name,
    color = color,
    isConfirmed = isConfirmed
)



