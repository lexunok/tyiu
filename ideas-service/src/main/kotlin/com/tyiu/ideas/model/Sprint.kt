package com.tyiu.ideas.model

import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import java.time.LocalDate

@Table
data class Sprint(
    @Id
    val id:String? = null,
    val projectId:String? = null,
    val name:String? = null,
    val goal:String? = null,
    val report:String? = null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = null,
    val workingHours:Long? = null,
    var status: SprintStatus? = SprintStatus.ACTIVE,
)

enum class SprintStatus{
    ACTIVE, DONE
}

data class SprintDTO(
    var id:String? = null,
    val projectId:String? = null,
    val name:String? = null,
    val report:String? = null,
    val status:SprintStatus? = null,
    val goal:String? = null,
    val startDate:LocalDate? = null,
    val finishDate:LocalDate? = null,
    val workingHours:Long? = null,
    var tasks:List<TaskDTO>? = null,
)

fun Sprint.toDTO(): SprintDTO = SprintDTO (
    id = id,
    projectId = projectId,
    goal = goal,
    name = name,
    status = status,
    report = report,
    startDate = startDate,
    finishDate = finishDate,
    workingHours = workingHours,
)