package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.Flow
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service

@Service
class SprintService (
    private val sprintRepository: SprintRepository,
    private val sprintMarksRepository: SprintMarksRepository,
    val template: R2dbcEntityTemplate,
)
{
    private fun sprintToDTO(sprint: Sprint): SprintDTO {
        return sprint.toDTO()
    }

    fun getAllSprintsByProject(projectId: String): Flow<SprintDTO> = sprintRepository.findAllSprintsByProject(projectId).map {sprintToDTO(it) }

    suspend fun getSprintById(id: String): SprintDTO? = sprintRepository.findById(id)?.let {sprintToDTO(it)}

    fun getActiveSprint(projectId: String): Flow<SprintDTO> = sprintRepository.findActiveSprint(projectId).map { sprintToDTO(it) }

    fun getAllSprintMarks(sprintId: String): Flow<SprintMarks> = sprintMarksRepository.findSprintMarks(sprintId)

    suspend fun updateSprintInfo(sprintId: String, sprintInfoRequest: SprintInfoRequest){
        val query =
            "UPDATE sprint SET name = :sprintName, goal = :sprintGoal, working_hours = :sprintWorkingHours WHERE id = :sprintId"
        return template.databaseClient.sql(query)
            .bind("sprintName",sprintInfoRequest.sprintName!!)
            .bind("sprintGoal",sprintInfoRequest.sprintGoal!!)
            .bind("sprintWorkingHours",sprintInfoRequest.sprintWorkingHours!!)
            .bind("sprintId",sprintId).await()
    }

    suspend fun changeSprintStatus(sprintStatusRequest: SprintStatusRequest){
        return template.databaseClient.sql("UPDATE sprint SET status = :sprintStatus WHERE id = :sprintId")
            .bind("sprintStatus",sprintStatusRequest.sprintStatus!!)
            .bind("sprintId",sprintStatusRequest.sprintId!!).await()
    }

    suspend fun createSprint(sprintDTO: SprintDTO): SprintDTO {
        val sprint = Sprint(
            projectId = sprintDTO.projectId,
            name = sprintDTO.name,
            goal = sprintDTO.goal,
            workingHours = sprintDTO.workingHours,
            )
        template.databaseClient.sql("UPDATE sprint SET status = 'DONE' WHERE status = 'ACTIVE'").await()
        return sprintRepository.save(sprint).toDTO()
    }

    suspend fun addSprintMarks(sprintId: String, sprintMarksRequest: SprintMarksRequest): SprintMarks {
        val sprintMarks = SprintMarks(
            sprintId = sprintId,
            userId = sprintMarksRequest.userId,
            mark = sprintMarksRequest.mark,
            )
        return sprintMarksRepository.save(sprintMarks)
    }

    suspend fun putSprintFinish(sprintId: String, sprintFinishRequest: SprintFinishRequest) {
        val query = "UPDATE sprint SET finish_date = :finishDate, report = :sprintReport, status = 'DONE' WHERE id = :sprintId"
        return template.databaseClient.sql(query)
            .bind("finishDate", sprintFinishRequest.finishDate!!)
            .bind("sprintReport", sprintFinishRequest.sprintReport!!)
            .bind("sprintId", sprintId).await()
    }

    suspend fun deleteSprint(id: String) = sprintRepository.deleteById(id)
}