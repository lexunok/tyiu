package com.tyiu.scrumservice.model


import com.tyiu.ideas.model.Comment
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
    @Query("SELECT * FROM task WHERE project_id = :projectId ORDER BY start_date ASC") // ПОИСК ТАСКА ПО ПРОЕКТУ. СОРТИРОВКА ПО СОЗДАНИЮ ТАСКА
    fun findAllByProjectId(projectId: String): Flow<Task>
    @Query(" SELECT * FROM task WHERE project_id = :projectId and status = 'InBacklog'") // ПРОСМОТР ТАСКОВ В БЭКЛОГЕ ПРОЕКТА
    fun findAllInBacklog(projectId: String): Flow<Task>

    @Query(" SELECT * FROM task WHERE project_id = :projectId and sprint_id = :sprintid ") // ПРОСМОТР ТАСКОВ В СПРИНТЕ ПРОЕКТА
    fun findAllTaskBySprint(projectId: String, sprintId: String): Flow<Task>

    @Query("SELECT * FROM task WHERE id = :id ") // ПОИСК ТАСКА ПО ЕГО АЙДИ
    fun findTaskById(id: BigInteger): Flow<Task>

    // УДАЛЕНИЕ ТАСКА ПО ЕГО АЙДИ. СДЕЛАЛ ОТДЕЛЬНЫЙ ЗАПРОС, ПОТОМУ ЧТО У НАС ID: STRING А В ТАБЛИЦЕ ID: BIGINTEGER. НЕ ПОНИМАЮ ПОЧЕМУ МЫ СДЕЛАЛИ ID = STRING
    @Query("delete FROM task WHERE id = :id ")
    fun deleteTaskById(id: BigInteger): Flow<Task>

    /*@Query("SELECT * FROM task WHERE status = :status")  ПОИСК ТАСКА ПО ТЕГУ может пригодится в будущем
    fun findAllInBacklog(status: String): Flow<Task>*/
}

@Table
data class Task (
    @Id
    val id: String? = null,
    val sprintId: String? = null,
    val projectId: String? = null,

    val name: String? = null,
    val description: String? = null,

    val initiatorId: String? = null,
    val executorId: String? = null,

    val workHour: Long? = null,

    val startDate: LocalDate? = LocalDate.now(),
    val finishDate: LocalDate? = LocalDate.now(),

    var status: TaskStatus
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
    val finishDate: LocalDate? = LocalDate.now(),

    var tag: TaskTagDTO? = null,
    var status: TaskStatus
)

enum class TaskStatus
{
    InBacklog, OnModification, New, InProgress, OnVerification, Done
}


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

/*fun User.toDTO(): UserDTO = UserDTO (
    id,
    email,
    firstName,
    lastName,
)*/
