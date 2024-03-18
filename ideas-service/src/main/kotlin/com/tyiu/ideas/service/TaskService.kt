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
    private val taskTagRepository: TaskTagRepository,
    private val task2tagRepository: Task2TagRepository,
    val template: R2dbcEntityTemplate
    )
{
    suspend fun taskToDTO(task: Task): TaskDTO
    {
        val tasks = task.toDTO()

        tasks.initiator = (task.initiatorId?.let{userRepository.findById(it)})?.toDTO()
        tasks.executor = (task.executorId?.let{userRepository.findById(it)})?.toDTO()
        tasks.tag = (task.id?.let{taskTagRepository.findAllByTaskId(it)})?.toList()
        return tasks
    }

    //get
    fun getAllTasksByProject(projectId: String): Flow<TaskDTO> = repository.findAllByProjectId(projectId).map {taskToDTO(it)}

    fun getAllTasksInBacklog(projectId: String): Flow<TaskDTO> = repository.findAllInBacklog(projectId).map {taskToDTO(it)}

    fun getAllTasksInSprint(projectId: String, sprintId: String): Flow<TaskDTO> =  repository.findAllTaskBySprint(projectId, sprintId).map {taskToDTO(it)}

    fun getOneTaskById(id: String): Flow<TaskDTO> = repository.findTaskById(id).map {taskToDTO(it)}

    //post
    suspend fun createTask(taskCreateRequest: TaskCreateRequest): TaskDTO {
        val task = Task (
            name = taskCreateRequest.name,
            description = taskCreateRequest.description,
            projectId = taskCreateRequest.projectId,
            workHour = taskCreateRequest.workHour,
            initiatorId = taskCreateRequest.initiatorId
        )
        if (taskCreateRequest.sprintId == null){
            task.status = TaskStatus.InBacklog
        }
        else {
            task.sprintId = taskCreateRequest.sprintId
            task.status = TaskStatus.New
        }
        val taskSave = taskToDTO(repository.save(task))

        task2tagRepository.save (
            Task2Tag(
            taskId = taskSave.id,
            tagId = "0aeadd54-5323-40b4-97bd-7e5072f9d576"
        )
        )
       return taskSave
    }

    //put
    suspend fun putUpdateTask(taskId: String, taskInfoRequest: TaskInfoRequest) {
        val query =
            "UPDATE task SET name = :taskName, description = :taskDescription, work_hour = :taskWork_hour WHERE id = :taskId"
        return template.databaseClient.sql(query)
            .bind("taskName", taskInfoRequest.taskName!!)
            .bind("taskDescription", taskInfoRequest.taskDescription!!)
            .bind("taskWork_hour", taskInfoRequest.taskWork_hour!!)
            .bind("taskId", taskId).await()
    }

    suspend fun putTaskStatus(TaskStatusRequest: TaskStatusRequest) {
        val query = "UPDATE task SET status = :taskStatus, executor_Id = :taskExecutor WHERE id = :taskId"
        return template.databaseClient.sql(query)
            .bind("taskStatus", TaskStatusRequest.taskStatus.toString())
            .bind("taskExecutor", TaskStatusRequest.taskExecutor.toString())
            .bind("taskId", TaskStatusRequest.taskId!!).await()
    }

    //delete
    suspend fun deleteTask(id: String) = repository.deleteById(id)

}
