package com.tyiu.scrumservice.model

import jdk.javadoc.internal.tool.Start
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface TaskMovementLogRepository: CoroutineCrudRepository<TaskMovementLog, String>

@Table
data class TaskMovementLog (
    @Id
    val id: String? = null,
    val projectId: String? = null,
    val taskId: String? = null,

    val executorId: String? = null,
    val userId: String? = null,

    val editDate: LocalDate? = null,
    val startDate: LocalDate? = null,
    val finishDate: LocalDate? = null
)
