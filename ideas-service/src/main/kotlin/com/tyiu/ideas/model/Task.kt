package com.tyiu.ideas.model

import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import org.springframework.core.task.TaskExecutor
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface TaskRepository: CoroutineCrudRepository<Task, String>
{
    //вывод tasktagDTO как списка
    //убрать лишние запросы
    @Query("SELECT * FROM task WHERE project_id = :projectId ORDER BY start_date ASC") // ПОИСК ТАСКА ПО ПРОЕКТУ. СОРТИРОВКА ПО СОЗДАНИЮ ТАСКА
    fun findAllByProjectId(projectId: String): Flow<Task>
    @Query(" SELECT * FROM task WHERE project_id = :projectId and status = 'InBacklog'") // ПРОСМОТР ТАСКОВ В БЭКЛОГЕ ПРОЕКТА
    fun findAllInBacklog(projectId: String): Flow<Task>

    @Query(" SELECT * FROM task WHERE sprint_id = :sprintId ") // ПРОСМОТР ТАСКОВ В СПРИНТЕ ПРОЕКТА
    fun findAllTaskBySprintId(sprintId: String): Flow<Task>

    @Query("SELECT * FROM task WHERE executor_id = :executorId ") // ПОИСК ТАСКА ПО ЕГО АЙДИ
    fun findTaskByExecutorId(executorId: String): Flow<Task>

    @Query("SELECT COUNT(*) FROM task WHERE project_id = :projectId AND status = 'InBackLog'")
    fun countTaskByProjectId(projectId: String): Flow<Int>
}

@Table
data class Task (
    @Id
    val id: String? = null,
    var sprintId: String? = null,
    val projectId: String? = null,

    var position:Int? = null,
    val name: String? = null,
    val description: String? = null,
    var leaderComment: String? = null,

    var initiatorId: String? = null,
    var executorId: String? = null,

    val workHour: Int? = null,

    val startDate: LocalDate? = LocalDate.now(),
    val finishDate: LocalDate? = null,

    var status: TaskStatus? = null
)

data class TaskDTO (
    val id: String? = null,

    var sprintId: String? = null,
    val projectId: String? = null,

    var position:Int? = null,
    val name:String? = null,
    val description: String? = null,
    var leaderComment: String? = null,

    var initiator: UserDTO? = null,
    var executor: UserDTO? = null,

    val workHour: Int? = null,

    val startDate: LocalDate? = null,
    val finishDate: LocalDate? = null,

    var tags: List<TagDTO>? = null,
    var status: TaskStatus? = null
)

fun Task.toDTO(): TaskDTO = TaskDTO (
    id = id,

    sprintId = sprintId,
    projectId = projectId,

    position = position,
    name = name,
    description = description,
    leaderComment = leaderComment,

    workHour = workHour,

    startDate = startDate,
    finishDate = finishDate,

    status = status
)

enum class TaskStatus {
    InBackLog, OnModification, NewTask, InProgress, OnVerification, Done
}
