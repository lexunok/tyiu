package com.tyiu.ideas.model

import com.tyiu.ideas.model.dto.UserDTO
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import java.time.LocalDate

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

    val workHour: Double? = null,

    val startDate: LocalDate? = LocalDate.now(),
    val finishDate: LocalDate? = null,

    var status: TaskStatus? = null
)

data class TaskDTO (
    var id: String? = null,

    var sprintId: String? = null,
    val projectId: String? = null,

    var position:Int? = null,
    var name:String? = null,
    var description: String? = null,
    var leaderComment: String? = null,

    var initiator: UserDTO? = null,
    var executor: UserDTO? = null,

    val workHour: Double? = null,

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
