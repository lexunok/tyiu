package com.tyiu.ideas.model

import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table

@Table
data class SprintMark(
    @Id
    val id:String? = null,
    val projectId:String? = null,
    val sprintId:String? = null,
    val userId:String? = null,
    val projectRole: ProjectRole,
    val mark:Double? = null,
    val countCompletedTasks:Int? = null
)
data class SprintMarkDTO(
    val id: String? = null,
    val projectId:String? = null,
    val sprintId: String? = null,
    val userId: String? = null,
    var firstName: String? = null,
    var lastName: String? = null,
    val projectRole: ProjectRole,
    val mark:Double? = null,
    val countCompletedTasks:Int? = null
)

data class SprintMarkRequest(
    val id: String? = null,
    val projectId:String? = null,
    val sprintId: String? = null,
    val userId: String? = null,
    var firstName: String? = null,
    var lastName: String? = null,
    val projectRole: ProjectRole,
    val mark:Double? = null,
    val tasks: List<TaskDTO>? = null
)