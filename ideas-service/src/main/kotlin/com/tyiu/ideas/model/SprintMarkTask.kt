package com.tyiu.ideas.model

import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface SprintMarkTaskRepository: CoroutineCrudRepository<SprintMarkTask, String>

@Table
data class SprintMarkTask(
    val sprintMarkId: String,
    val taskId: String
)