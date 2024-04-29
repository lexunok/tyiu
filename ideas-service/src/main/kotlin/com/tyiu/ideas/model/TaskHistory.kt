package com.tyiu.ideas.model

import org.springframework.data.relational.core.mapping.Table

@Table
data class TaskHistory (
    val taskId: String? = null,
    val sprintId: String? = null,
    val status: TaskStatus? = null,
    val executorId: String? = null,
)