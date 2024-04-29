package com.tyiu.ideas.model

import org.springframework.data.relational.core.mapping.Table

@Table
data class SprintMarkTask(
    val sprintMarkId: String,
    val taskId: String
)