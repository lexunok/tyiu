package com.tyiu.scrumservice.service

import com.tyiu.ideas.model.CommentDTO
import com.tyiu.ideas.model.toDTO
import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service
import java.math.BigInteger

@Service
class TaskService(private val repository: TaskRepository){

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

    fun deleteTask(id: BigInteger) = repository.deleteTaskById(id)

    /*fun getAllTasksInBacklog(status: String): Flow<TaskDTO> =  ПОИСК ТАСКА ПО ТЕГУ может пригодится в будущем
    repository.findAllInBacklog(status).map {
    t -> val task = t.toDTO()
    return@map task
    }*/

}
