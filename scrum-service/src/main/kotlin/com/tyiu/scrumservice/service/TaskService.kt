package com.tyiu.scrumservice.service

import com.tyiu.ideas.model.CommentDTO
import com.tyiu.ideas.model.toDTO
import com.tyiu.scrumservice.model.Task
import com.tyiu.scrumservice.model.TaskDTO
import com.tyiu.scrumservice.model.TaskRepository
import com.tyiu.scrumservice.model.toDTO
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service

@Service
class TaskService(private val repository: TaskRepository){

    /*fun getAllTasks(projectId: String): Flow<TaskDTO> =
        repository.findAllByProjectId(projectId).map {
            t -> val task = t.toDTO()
            return@map task
        }*/

    fun getAllProjects(): Flow<Task> = repository.findAll()
}
