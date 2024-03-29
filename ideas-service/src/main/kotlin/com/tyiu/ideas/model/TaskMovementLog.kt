package com.tyiu.ideas.model

import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

//автоматическое создание лога таска при создании таска
//автоматическое измение времени при изменении статуса
interface TaskMovementLogRepository: CoroutineCrudRepository<TaskMovementLog, String>{
    @Query("SELECT * FROM task_movement_log WHERE task_id = :taskId")
    fun findAllByTaskId(taskId: String): Flow<TaskMovementLog>

    suspend fun existsTaskMovementLogByTaskId(taskId: String): Boolean
}

@Table
data class TaskMovementLog (
    @Id
    val id: String? = null,
    val taskId: String? = null,
    val executorId: String? = null,
    val userId: String? = null,
    val startDate: LocalDate? = LocalDate.now(),
    val endDate: LocalDate? = null,
    var status: TaskStatus? = null
)

data class TaskMovementLogDTO (
    val id: String? = null, //айди лога
    var task: TaskDTO? = null, // связь с таском
    var executor: UserDTO? = null, // отвественный за таск
    var user: UserDTO? = null,  // Тот, кто поменял статус таска
    val startDate: LocalDate? = null, // при создании таска - LocalDate.Now. При имзенении статуса таска LocalDate.Now
    val endDate: LocalDate? = null, // при создании null. При имзенении статуса таска LocalDate.Now
    var status: TaskStatus? = null // предыдущий статус таска ????
)

fun TaskMovementLog.toDTO(): TaskMovementLogDTO = TaskMovementLogDTO (
    id = id,
    startDate = startDate,
    endDate = endDate,
    status = status
)
