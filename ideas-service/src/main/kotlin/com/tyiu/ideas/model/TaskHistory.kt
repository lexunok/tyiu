package com.tyiu.ideas.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface TaskHistoryRepository: CoroutineCrudRepository<TaskHistory, String>

@Table
data class TaskHistory (
    val taskId: String? = null,
    val sprintId: String? = null,
    val status: TaskStatus? = null,
    val executorId: String? = null,
)