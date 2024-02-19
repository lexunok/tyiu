package com.tyiu.scrumservice.model

import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface TaskMovementLogRepository: CoroutineCrudRepository<TaskMovementLog, String>{
    @Query("SELECT * FROM task_movement_log WHERE task_id = :taskId")
    fun findAllBytaskId(taskId: String): Flow<TaskMovementLog>
}


@Table
data class TaskMovementLog (
    @Id
    val id: String? = null,
    val projectId: String? = null,
    val taskId: String? = null,
    val executorId: String? = null,
    val userId: String? = null,
    val startDate: LocalDate? = LocalDate.now(),
    val endDate: LocalDate? = null,
    var status: TaskStatus? = null // предыдущий статус
)

data class TaskMovementLogDTO (
    val id: String? = null,
    val projectId: String? = null,
    val task: TaskDTO? = null,
    val executor: UserDTO? = null,
    val user: UserDTO? = null,
    val startDate: LocalDate? = LocalDate.now(),
    val endDate: LocalDate? = null,
    var status: TaskStatus? = null
)

fun TaskMovementLog.toDTO(): TaskMovementLogDTO = TaskMovementLogDTO (
    id = id,
    projectId = projectId,
    startDate = startDate,
    endDate = endDate,
    status = status
)
