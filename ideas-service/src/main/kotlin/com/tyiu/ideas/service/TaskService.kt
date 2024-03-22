package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.toList
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service

@Service
class TaskService
    (
    private val repository: TaskRepository,
    private val userRepository: UserRepository,
    private val tagRepository: TagRepository,
    private val task2tagRepository: Task2TagRepository,
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

    fun getOneTaskById(id: String): Flow<TaskDTO> = repository.findTaskById(id).map {taskToDTO(it)}

    //post
    suspend fun createTask(taskDTO: TaskDTO): TaskDTO {
        val task = Task (
            sprintId = taskDTO.sprintId,
            projectId = taskDTO.projectId,
            position = taskDTO.position,
            name = taskDTO.name,
            description = taskDTO.description,
            leaderComment = taskDTO.leaderComment,
            initiatorId = taskDTO.initiator?.id,
            workHour = taskDTO.workHour,
            status = if (taskDTO.sprintId == null) TaskStatus.InBackLog else TaskStatus.NewTask
        )

        val taskSave = taskToDTO(repository.save(task))

        taskDTO.tags?.forEach {
            task2tagRepository.save (
                Task2Tag(
                    taskId = taskSave.id,
                    tagId = it.id
                )
            )
        }
        return taskSave
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

    suspend fun putTaskStatus(taskDTO: TaskDTO) {
        val query = "UPDATE task SET status = :status, executor_Id = :executor WHERE id = :taskId"
        return template.databaseClient.sql(query)
            .bind("status", taskDTO.status!!.toString())
            .bind("executor", taskDTO.executor?.id!!)
            .bind("taskId", taskDTO.id!!).await()
    }

    suspend fun putUpdateExecutorTask(taskId: String, executorId: String){
        return template.databaseClient
            .sql("UPDATE task SET executor_id = :executorId WHERE id = :taskId")
            .bind("executorId", executorId)
            .bind("taskId", taskId).await()
    }

    //delete
    suspend fun deleteTask(id: String) = repository.deleteById(id)

}
