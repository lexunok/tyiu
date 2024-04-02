package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.entities.User
import kotlinx.coroutines.flow.*
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import java.time.LocalDateTime

@Service
class SprintService (
    private val sprintRepository: SprintRepository,
    private val sprintMarksRepository: SprintMarksRepository,
    private val taskRepository: TaskRepository,
    private val taskMovementLogRepository: TaskMovementLogRepository,
    private val tagRepository: TagRepository,
    val template: R2dbcEntityTemplate,
    private val userRepository: UserRepository
)
{
    private suspend fun sprintToDTO(sprint: Sprint): SprintDTO {
        val sprintDTO = sprint.toDTO()
        sprintDTO.tasks = sprint.id?.let { taskRepository.findAllTaskBySprintId(it) }?.map {
            val taskDTO = it.toDTO()
            taskDTO.tags = it.id?.let { it1 -> tagRepository.findAllTagByTaskId(it1) }?.map { it1 -> it1.toDTO() }?.toList()
            taskDTO.initiator = it.initiatorId?.let { userRepository.findById(it) }?.toDTO()
            taskDTO.executor = it.executorId?.let { userRepository.findById(it) }?.toDTO()
            return@map taskDTO
        }?.toList()
        return sprintDTO
    }

    fun getAllSprintsByProject(projectId: String): Flow<SprintDTO> = sprintRepository.findAllSprintsByProject(projectId).map {sprintToDTO(it) }

    suspend fun getSprintById(id: String): SprintDTO? = sprintRepository.findById(id)?.let { sprintToDTO(it) }

    suspend fun getActiveSprint(projectId: String): SprintDTO = sprintToDTO(sprintRepository.findActiveSprint(projectId))

    fun getAllSprintMarks(sprintId: String): Flow<SprintMarks> = sprintMarksRepository.findSprintMarks(sprintId)

    suspend fun createSprint(sprintDTO: SprintDTO, user: User): SprintDTO {
        sprintDTO.projectId?.let {
            template.databaseClient
                .sql("UPDATE sprint SET status = 'DONE' WHERE status = 'ACTIVE' AND project_id = :projectId")
                .bind("projectId", it)
                .await()
        }
        val createdSprint = sprintRepository.save(
            Sprint(
                projectId = sprintDTO.projectId,
                name = sprintDTO.name,
                goal = sprintDTO.goal,
                workingHours = sprintDTO.workingHours,
                startDate = sprintDTO.startDate,
                finishDate = sprintDTO.finishDate
            )
        )
        sprintDTO.tasks?.forEach {
            template.databaseClient
                .sql("UPDATE task SET sprint_id = :sprintId, status = 'NewTask' WHERE id = :taskId")
                .bind("sprintId", createdSprint.id!!)
                .bind("taskId", it.id!!)
                .await()
            template.databaseClient
                .sql("UPDATE task_movement_log SET end_date = :date WHERE task_id = :taskId AND end_date IS NULL")
                .bind("date", LocalDateTime.now())
                .bind("taskId", it.id)
                .await()
            taskMovementLogRepository.save(
                TaskMovementLog(
                    taskId = it.id,
                    executorId = it.executor?.id,
                    userId = user.id,
                    startDate = LocalDateTime.now(),
                    status = TaskStatus.NewTask
                )
            )
        }

        sprintDTO.projectId?.let {
            val tasks = taskRepository.findTaskByProjectIdAndStatusOrderByPosition(it,TaskStatus.InBackLog)
            var i = 1
            tasks.collect {task ->
                if (task.position != i){
                    task.id?.let { id -> taskRepository.updateTasksByProjectIdAndId(i, sprintDTO.projectId, id) }
                }
                i += 1
            }
        }

        return sprintToDTO(createdSprint)
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

    suspend fun putSprintFinishWithTaskTransfer(sprintId: String,sprintDTO: SprintDTO,user: User) {
        val tasks = taskRepository.findTasksNotDoneBySprintId(sprintId)
        val newSprint = createSprint(sprintDTO,user)
        tasks.toList().forEach { taskRepository.finishTaskWithTransfer(newSprint.id,it.id) }
        sprintRepository.finishSprintById(sprintId,sprintDTO.report)
    }

    suspend fun putSprintFinishWithoutTaskTransfer(sprintFinishRequest: SprintFinishRequest) {
        val tasks = sprintFinishRequest.sprintId?.let { taskRepository.findTasksNotDoneBySprintId(it) }
        var pos = tasks?.first()?.projectId?.let { taskRepository.countTaskByProjectId(it) }
        tasks?.toList()?.forEach {
            pos = pos?.plus(1)
            taskRepository.finishTaskWithoutTransfer(pos,it.id)
        }
        sprintRepository.finishSprintById(sprintFinishRequest.sprintId,sprintFinishRequest.sprintReport)
    }

    suspend fun deleteSprint(id: String) = sprintRepository.deleteById(id)
}