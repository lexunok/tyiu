package com.tyiu.scrumservice.model

import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.math.BigInteger
import java.time.LocalDate

interface TaskRepository: CoroutineCrudRepository<Task, String>
{
    //убрать лишние запросы ( изучить документацию )
    @Query("SELECT * FROM task WHERE project_id = :projectId ORDER BY start_date ASC") // ПОИСК ТАСКА ПО ПРОЕКТУ. СОРТИРОВКА ПО СОЗДАНИЮ ТАСКА
    fun findAllByProjectId(projectId: String): Flow<Task>
    @Query(" SELECT * FROM task WHERE project_id = :projectId and status = 'InBacklog'") // ПРОСМОТР ТАСКОВ В БЭКЛОГЕ ПРОЕКТА
    fun findAllInBacklog(projectId: String): Flow<Task>

    @Query(" SELECT * FROM task WHERE project_id = :projectId and sprint_id = :sprintId ") // ПРОСМОТР ТАСКОВ В СПРИНТЕ ПРОЕКТА
    fun findAllTaskBySprint(projectId: String, sprintId: String): Flow<Task>

    @Query("SELECT * FROM task WHERE id = :id ") // ПОИСК ТАСКА ПО ЕГО АЙДИ
    fun findTaskById(id: String): Flow<Task>
}

@Table
data class Task (
    @Id
    val id: String? = null,
    val sprintId: String? = null,
    val projectId: String? = null,
    //поле
    //val isInBacklog: Boolean? = true,
    val name: String? = null,
    val description: String? = null,

    val initiatorId: String? = null,
    val executorId: String? = null,

    val workHour: Long? = null,

    val startDate: LocalDate? = LocalDate.now(),
    val finishDate: LocalDate? = null,

    var status: TaskStatus? = TaskStatus.InBacklog,
)

data class Task2Sprint (
    val sprintId: String? = null,
    val taskId: String? = null
)

data class TaskDTO (
    val id: String? = null,
    
    val sprintId: String? = null, // Если можно сделать нулевое значение у ссылочного типа, иначе Task2Sprint
    val projectId: String? = null,

    val name:String? = null,
    val description: String? = null,

    val initiator: UserDTO? = null,
    val executor: UserDTO? = null, //firstName lastName

    val workHour: Long? = null,

    val startDate: LocalDate? = LocalDate.now(),
    val finishDate: LocalDate? = null,

    var tag: TaskTagDTO? = null,
    var status: TaskStatus? = TaskStatus.InBacklog,
)

enum class TaskStatus
{
    InBacklog, OnModification, New, InProgress, OnVerification, Done
}

data class TaskStatusRequest(
    val taskId :BigInteger? = null,
    val taskStatus: TaskStatus? = null,
)

data class taskInfoRequest(
    val taskId :BigInteger? = null,
    var taskName: String? = null,
    var taskDescription: String? = null,
    var taskWork_hour: Long? = null,
    var taskStatus: String? = null,
)


fun Task.toDTO(): TaskDTO = TaskDTO (
    id = id,
    sprintId = sprintId,

    projectId = projectId,

    name = name,
    description = description,

    workHour = workHour,

    startDate = startDate,
    finishDate = finishDate,

    status = status,
)

//СВЯЗАТЬ С User И TaskTag
