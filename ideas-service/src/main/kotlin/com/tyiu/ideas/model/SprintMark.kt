package com.tyiu.ideas.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface SprintMarkRepository: CoroutineCrudRepository<SprintMark, String>{

    fun findSprintMarksBySprintId(sprintId: String): Flow<SprintMark>

    @Query("SELECT * FROM sprint_mark WHERE project_id =:projectId and user_id =:userId")
    fun findSprintMarksByProjectIdAndUserId(projectId: String, userId: String?): Flow<SprintMark>
}
@Table
data class SprintMark(
    @Id
    val id:String? = null,
    val sprintId:String? = null,
    val userId:String? = null,
    val projectRole: ProjectRole,
    val mark:Double? = null,
)
data class SprintMarkDTO(
    val id: String? = null,
    val sprintId: String? = null,
    val userId: String? = null,
    var firstName: String? = null,
    var lastName: String? = null,
    val projectRole: ProjectRole,
    val mark:Double? = null,
    var tasks: List<TaskDTO>? = null,
)

fun SprintMark.toDTO(): SprintMarkDTO = SprintMarkDTO(
    id = id,
    sprintId = sprintId,
    userId = userId,
    projectRole = projectRole,
    mark = mark,
)