package com.tyiu.scrumservice.model

import jdk.javadoc.internal.tool.Start
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface TaskTagRepository: CoroutineCrudRepository<TaskTagRepository, String>

@Table
data class TaskTag ( //name color isconfirmed сделал VAR т.к посчитал, что они должны будут изменяться. могу ошибаться
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



