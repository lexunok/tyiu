package com.tyiu.ideas.model

import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface Task2TagRepository: CoroutineCrudRepository<Task2Tag, String>

@Table("task_tag")
data class Task2Tag (
    val taskId: String? = null,
    val tagId: String? = null
)
