package com.tyiu.scrumservice.service

import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import org.springframework.web.bind.annotation.RequestBody
import java.math.BigInteger

@Service
class TaskService
    (
    private val repository: TaskRepository,
    val template: R2dbcEntityTemplate
    )
{

    fun getAllTasksByProject(projectId: String): Flow<TaskDTO> =
        repository.findAllByProjectId(projectId).map {
            t -> val task = t.toDTO()
            return@map task
        }

    fun getAllTasksInBacklog(projectId: String): Flow<TaskDTO> =
        repository.findAllInBacklog(projectId).map {
            t -> val task = t.toDTO()
            return@map task
        }

    fun getAllTasksInSprint(projectId: String, sprintId: String): Flow<TaskDTO> =
        repository.findAllTaskBySprint(projectId, sprintId).map { t ->
            val task = t.toDTO()
            return@map task
        }

    fun getOneTaskById(id: BigInteger): Flow<TaskDTO> =
        repository.findTaskById(id).map { t ->
            val task = t.toDTO()
            return@map task
        }

    fun getAllTasks(): Flow<Task> = repository.findAll() // ВЫВОД ВСЕХ СОЗДАННЫХ ТАСКОВ ВО ВСЕХ ПРОЕКТАХ. НУЖНО НА ВРЕМЯ РАЗРАБОТКИ

    suspend fun createTask(taskDTO:TaskDTO):TaskDTO {
        val task = Task(
            name = taskDTO.name,
            description = taskDTO.description,
            status =  taskDTO.status, // inbacklog
            workHour = taskDTO.workHour
        )
        val taskToDTO = repository.save(task).toDTO()
        return taskToDTO
    }

    suspend fun putUpdateTask(taskId: BigInteger, taskInfoRequest: taskInfoRequest) {
        val query =
            "UPDATE task SET name = :taskName, description = :taskDescription, work_hour = :taskWork_hour WHERE id = :taskId"
        return template.databaseClient.sql(query)
            .bind("taskName", taskInfoRequest.taskName!!)
            .bind("taskDescription", taskInfoRequest.taskDescription!!)
            .bind("taskWork_hour", taskInfoRequest.taskWork_hour!!)
            .bind("taskId", taskId).await()
    }

    suspend fun putTaskStatus(TaskStatusRequest: TaskStatusRequest) {
        val query = "UPDATE task SET status = :taskStatus WHERE id = :taskId"
        return template.databaseClient.sql(query)
            .bind("taskStatus", TaskStatusRequest.taskStatus.toString())
            .bind("taskId", TaskStatusRequest.taskId!!).await()
    }

    fun deleteTask(id: BigInteger) = repository.deleteTaskById(id)


}
