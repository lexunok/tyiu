package com.tyiu.scrumservice.model

import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface ProjectMarksRepository: CoroutineCrudRepository<ProjectMarksRepository, String>
@Table
data class ProjectMarks(
        val projectId:String? = null,
        val userId:String? = null,
        val mark:Double? = null,
)