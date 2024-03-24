package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.toList
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service

@Service
class SprintService (
    private val sprintRepository: SprintRepository,
    private val sprintMarksRepository: SprintMarksRepository,
    private val taskRepository: TaskRepository,
    val template: R2dbcEntityTemplate,
)
{
    private fun sprintToDTO(sprint: Sprint): SprintDTO {
        return sprint.toDTO()
    }

    fun getAllSprintsByProject(projectId: String): Flow<SprintDTO> = sprintRepository.findAllSprintsByProject(projectId).map {sprintToDTO(it) }

    suspend fun getSprintById(id: String): SprintDTO? = sprintRepository.findById(id)?.let {sprintToDTO(it)}

    suspend fun getActiveSprint(projectId: String): SprintDTO = sprintRepository.findActiveSprint(projectId).map { sprintToDTO(it) }.first()

    fun getAllSprintMarks(sprintId: String): Flow<SprintMarks> = sprintMarksRepository.findSprintMarks(sprintId)

    suspend fun createSprint(sprintDTO: SprintDTO): SprintDTO {
        val sprint = Sprint(
            projectId = sprintDTO.projectId,
            name = sprintDTO.name,
            goal = sprintDTO.goal,
            workingHours = sprintDTO.workingHours,
            startDate = sprintDTO.startDate,
            finishDate = sprintDTO.finishDate
        )
        template.databaseClient
            .sql("UPDATE sprint SET status = 'DONE' WHERE status = 'ACTIVE'")
            .await()
        val createdSprint = sprintRepository.save(sprint).toDTO()
        sprintDTO.tasks?.forEach {
            template.databaseClient
                .sql("UPDATE task SET sprint_id = :sprintId WHERE id = :taskId")
                .bind("sprintId", createdSprint.id!!)
                .bind("taskId", it.id!!)
                .await()
        }
        createdSprint.tasks = createdSprint.id?.let { taskRepository.findAllTaskBySprintId(it) }?.map { it.toDTO() }?.toList()
        return createdSprint
    }

    suspend fun addSprintMarks(sprintId: String, sprintMarksRequest: SprintMarksRequest): SprintMarks {
        val sprintMarks = SprintMarks(
            sprintId = sprintId,
            userId = sprintMarksRequest.userId,
            mark = sprintMarksRequest.mark,
            )
        return sprintMarksRepository.save(sprintMarks)
    }

    //TODO: добавить изменение списка задач
    suspend fun updateSprintInfo(sprintId: String, sprintDTO: SprintDTO){
        val query =
            "UPDATE sprint SET name = :name, goal = :goal, start_date = :start_date, finish_date = :finish_date, working_hours = :working_hours WHERE id = :sprintId"
        return template.databaseClient.sql(query)
            .bind("name", sprintDTO.name!!)
            .bind("goal", sprintDTO.goal!!)
            .bind("start_date", sprintDTO.startDate!!)
            .bind("finish_date", sprintDTO.finishDate!!)
            .bind("working_hours", sprintDTO.workingHours!!)
            .bind("sprintId", sprintId).await()
    }

    suspend fun changeSprintStatus(sprintId: String, status: SprintStatus){
        return template.databaseClient.sql("UPDATE sprint SET status = :sprintStatus WHERE id = :sprintId")
            .bind("sprintStatus", status)
            .bind("sprintId", sprintId).await()
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