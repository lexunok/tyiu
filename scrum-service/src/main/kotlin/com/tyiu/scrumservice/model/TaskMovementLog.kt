package com.tyiu.scrumservice.model

import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

//реализовать:
//автоматическое создание лога таска при создании таска
//автоматическое измение времени и статуса
interface TaskMovementLogRepository: CoroutineCrudRepository<TaskMovementLog, String>{
    @Query("SELECT * FROM task_movement_log WHERE task_id = :taskId")
    fun findAllByTaskId(taskId: String): Flow<TaskMovementLog>
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
    var status: TaskStatus? = null
)

data class TaskMovementLogDTO (
    val id: String? = null, //айди лога
    val projectId: String? = null, // связь с проектом
    val task: TaskDTO? = null, // связь с таском
    val executor: UserDTO? = null, // отвественный за таск
    val user: UserDTO? = null,  // Тот, кто поменял статус таска
    val startDate: LocalDate? = LocalDate.now(), // при создании таска - LocalDate.Now. При имзенении статуса таска LocalDate.Now
    val endDate: LocalDate? = null, // при создании null. При имзенении статуса таска LocalDate.Now
    var status: TaskStatus? = null // предыдущий статус таска ????
)

fun TaskMovementLog.toDTO(): TaskMovementLogDTO = TaskMovementLogDTO (
    id = id,
    projectId = projectId,
    startDate = startDate,
    endDate = endDate,
    status = status
)
//CREATE TABLE task_movement_log (id TEXT DEFAULT gen_random_uuid()::TEXT PRIMARY KEY, project_id TEXT, start_date DATE, end_date DATE, status TEXT);