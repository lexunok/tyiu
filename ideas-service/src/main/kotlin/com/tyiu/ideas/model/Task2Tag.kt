package com.tyiu.ideas.model

import org.springframework.data.relational.core.mapping.Table

@Table("task_tag")
data class Task2Tag (
    val taskId: String? = null,
    val tagId: String? = null
)
