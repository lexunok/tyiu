package com.tyiu.scrumservice.model


import com.tyiu.scrumservice.model.dto.UserDTO
import com.tyiu.scrumservice.model.entities.User
import jdk.javadoc.internal.tool.Start
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface TaskRepository: CoroutineCrudRepository<Task, String>
{
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
    InBackLog, OnModification, New, InProgress, OnVerification, Done
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
