package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.entities.User
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.toList
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import java.time.LocalDate

@Service
class TaskService
    (
    private val repository: TaskRepository,
    private val userRepository: UserRepository,
    private val tagRepository: TagRepository,
    private val task2tagRepository: Task2TagRepository,
    private val taskMovementLogRepository: TaskMovementLogRepository,
    val template: R2dbcEntityTemplate
    )
{
    suspend fun taskToDTO(task: Task): TaskDTO
    {
        val tasks = task.toDTO()

        tasks.initiator = (task.initiatorId?.let{userRepository.findById(it)})?.toDTO()
        tasks.executor = (task.executorId?.let{userRepository.findById(it)})?.toDTO()
        tasks.tags = task.id?.let { tagRepository.findAllTagByTaskId(it).toList().map { tag -> tag.toDTO() } }
        return tasks
    }

    //get
    fun getAllTasksByProject(projectId: String): Flow<TaskDTO> = repository.findAllByProjectId(projectId).map {taskToDTO(it)}

    fun getAllTasksInBacklog(projectId: String): Flow<TaskDTO> = repository.findAllInBacklog(projectId).map {taskToDTO(it)}

    fun getAllTasksInSprint(sprintId: String): Flow<TaskDTO> =  repository.findAllTaskBySprintId(sprintId).map {taskToDTO(it)}

    suspend fun getOneTaskById(id: String): TaskDTO? = repository.findById(id)?.let { taskToDTO(it) }

    //post
    suspend fun createTask(taskDTO: TaskDTO, userId: String): TaskDTO {
        val createdTask = repository.save(
            Task (
                sprintId = taskDTO.sprintId,
                projectId = taskDTO.projectId,
                position = taskDTO.projectId?.let { repository.countTaskByProjectId(it) }?.first()?.plus(1),
                name = taskDTO.name,
                description = taskDTO.description,
                leaderComment = taskDTO.leaderComment,
                initiatorId = userId,
                workHour = taskDTO.workHour,
                status = if (taskDTO.sprintId == null) TaskStatus.InBackLog else TaskStatus.NewTask
            )
        )

        taskMovementLogRepository.save(
            TaskMovementLog(
                taskId = createdTask.id,
                executorId = taskDTO.executor?.id,
                userId = taskDTO.initiator?.id,
                startDate = LocalDate.now(),
                status = createdTask.status
            )
        )

        taskDTO.tags?.forEach {
            task2tagRepository.save (
                Task2Tag(
                    taskId = createdTask.id,
                    tagId = it.id
                )
            )
        }
        return taskToDTO(createdTask)
    }

    //put
    suspend fun putUpdateTask(taskId: String, taskDTO: TaskDTO) {
        val query =
            "UPDATE task SET name = :name, description = :description, work_hour = :workHour WHERE id = :taskId"
        return template.databaseClient.sql(query)
            .bind("name", taskDTO.name!!)
            .bind("description", taskDTO.description!!)
            .bind("workHour", taskDTO.workHour!!)
            .bind("taskId", taskId).await()
    }

    suspend fun putUpdateExecutorTask(taskId: String, executorId: String){
        return template.databaseClient
            .sql("UPDATE task SET executor_id = :executorId WHERE id = :taskId")
            .bind("executorId", executorId)
            .bind("taskId", taskId).await()
    }

    //delete
    suspend fun deleteTask(id: String) = repository.deleteById(id)

    suspend fun changePosition(taskId: String, position: Int){
        repository.findById(taskId).let {
            template.databaseClient
                .sql("UPDATE task SET position = :position WHERE id = :taskId")
                .bind("position", position)
                .bind("taskId", taskId)
                .await()
            if (it?.position!! > position){
                template.databaseClient
                    .sql("UPDATE task SET position = position + 1 WHERE project_id = :projectId AND id <> :taskId AND position < :position AND position >= :newPosition")
                    .bind("projectId", it.projectId!!)
                    .bind("taskId", taskId)
                    .bind("position", it.position!!)
                    .bind("newPosition", position)
                    .await()
            }
            else if (it.position!! < position){
                template.databaseClient
                    .sql("UPDATE task SET position = position - 1 WHERE project_id = :projectId AND id <> :taskId AND position > :position AND position <= :newPosition")
                    .bind("projectId", it.projectId!!)
                    .bind("taskId", taskId)
                    .bind("position", it.position!!)
                    .bind("newPosition", position)
                    .await()
            }
        }
    }

}
