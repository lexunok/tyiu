package com.tyiu.scrumservice.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.math.BigInteger
import java.time.LocalDate

interface SprintMarksRepository: CoroutineCrudRepository<SprintMarks, String>{
    @Query("SELECT * FROM sprint_marks WHERE sprint_id =:sprintId")
    fun findSprintMarks(sprintId: String): Flow<SprintMarks>
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

data class sprintMarksRequest(
        val userId: String? = null,
        val mark: Double? = null,
)

fun SprintMarks.toDTO():SprintMarksDTO = SprintMarksDTO(
        sprintId = sprintId,
        userId = userId,
        mark = mark,
)