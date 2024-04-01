package com.tyiu.ideas.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface SprintMarksRepository: CoroutineCrudRepository<SprintMarks, String>{
    @Query("SELECT * FROM sprint_marks WHERE sprint_id =:sprintId")
    fun findSprintMarks(sprintId: String): Flow<SprintMarks>

    @Query("SELECT * FROM sprint_marks WHERE project_id =:projectId and user_id =:userId")
    fun findSprintMarksByProjectIdAAndUserId(projectId: String, userId: String?): Flow<SprintMarks>
}
@Table

data class SprintMarks(
    val projectId:String? = null,
    val sprintId:String? = null,
    val userId:String? = null,
    val mark:Double? = null,
)
data class SprintMarksDTO(
    val userId: String? = null,
    val sprintId: String? = null,
    var firstName: String? = null,
    var lastName: String? = null,
    val mark:Double? = null,
    // tasks?
)

data class SprintMarksRequest(
    val userId: String? = null,
    val mark: Double? = null,
)

fun SprintMarks.toDTO(): SprintMarksDTO = SprintMarksDTO(
    sprintId = sprintId,
    userId = userId,
    mark = mark,
)