package com.tyiu.ideas.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface Task2TagRepository: CoroutineCrudRepository<Task2Tag, String> {
}

@Table("task_to_tag")
data class Task2Tag (
    val taskId: String? = null,
    val tagId: String? = null
)
