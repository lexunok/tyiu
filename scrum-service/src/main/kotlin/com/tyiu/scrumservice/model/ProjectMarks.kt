package com.tyiu.scrumservice.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.math.BigInteger


interface ProjectMarksRepository: CoroutineCrudRepository<ProjectMarks, String>{
        @Query("SELECT * FROM project_marks WHERE project_id = :projectId")
        fun findMarksByProjectId(projectId: String): Flow<ProjectMarks>
}
@Table
data class ProjectMarks(
        val projectId:String? = null,
        val userId:String? = null,
        val mark:Double? = null,
)