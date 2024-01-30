package com.tyiu.scrumservice.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.math.BigInteger
import java.time.LocalDate

interface TaskMovementLogRepository: CoroutineCrudRepository<TaskMovementLog, String>{
    @Query("SELECT * FROM task_movement_log WHERE project_id = :projectId")
    fun findLogByProjectId(projectId: String): Flow<TaskMovementLog>
}


@Table
data class TaskMovementLog (
    @Id
    val id: String? = null,
    val projectId: String? = null,
    val taskId: String? = null,
    val executorId: String? = null,
    val userId: String? = null,
    val editDate: LocalDate? = null,
    val taskStatusA: TaskStatus? = null,
    val taskStatusB: TaskStatus? = null,
)
