package com.tyiu.ideas.model

import com.tyiu.ideas.model.dto.UserDTO
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import java.time.LocalDateTime

@Table
data class TaskMovementLog (
    @Id
    val id: String? = null,
    val taskId: String? = null,
    val executorId: String? = null,
    val userId: String? = null,
    val startDate: LocalDateTime? = LocalDateTime.now(),
    val endDate: LocalDateTime? = null,
    var status: TaskStatus? = null
)

data class TaskMovementLogDTO (
    var id: String? = null,
    var task: TaskDTO? = null,
    var executor: UserDTO? = null,
    var user: UserDTO? = null,
    var startDate: LocalDateTime? = null,
    val endDate: LocalDateTime? = null,
    var status: TaskStatus? = null
)

fun TaskMovementLog.toDTO(): TaskMovementLogDTO = TaskMovementLogDTO (
    id = id,
    startDate = startDate,
    endDate = endDate,
    status = status
)
