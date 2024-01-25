package com.tyiu.scrumservice.model

import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface SprintMarksRepository: CoroutineCrudRepository<SprintMarksRepository, String>
@Table

data class SprintMarks(
        val projectId:String? = null,
        val sprintId:String? = null,
        val userId:String? = null,
        val mark:Long? = null,
)