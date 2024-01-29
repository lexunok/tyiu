package com.tyiu.scrumservice.model

import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface TaskTagRepository: CoroutineCrudRepository<TaskTag, String>{}

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



