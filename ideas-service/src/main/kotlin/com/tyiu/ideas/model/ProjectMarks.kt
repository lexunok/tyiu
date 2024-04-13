package com.tyiu.ideas.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository


interface ProjectMarksRepository: CoroutineCrudRepository<ProjectMarks, String>{
        @Query("SELECT * FROM project_marks WHERE project_id = :projectId")
        fun findMarksByProjectId(projectId: String): Flow<ProjectMarks>

        suspend fun existsByUserIdAndProjectId(userId: String?, projectId: String?): Boolean
}
@Table
data class ProjectMarks(
        val projectId:String? = null,
        val userId:String? = null,
        val mark:Double? = null,
)

data class ProjectMarksDTO(
        val projectId:String? = null,
        val userId: String? = null,
        var firstName: String? = null,
        var lastName: String? = null,
        var projectRole: ProjectRole = ProjectRole.MEMBER,
        val mark:Double? = null,
        var tasks: List<TaskDTO>? = null,
)

fun ProjectMarks.toDTO(): ProjectMarksDTO = ProjectMarksDTO(
        projectId = projectId,
        userId = userId,
        mark = mark
)