package com.tyiu.scrumservice.controller

import com.tyiu.ideas.model.CommentDTO
import com.tyiu.ideas.model.toDTO
import com.tyiu.scrumservice.model.Task
import com.tyiu.scrumservice.model.TaskDTO
import com.tyiu.scrumservice.service.TaskService
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.web.bind.annotation.*
@RestController
@RequestMapping("/api/v1/scrum-service/task")
class TaskController (private val taskService: TaskService  ) {

    /*@GetMapping("{projectId}/all")
    fun getAllTask(@PathVariable projectId: String): Flow<TaskDTO> = taskService.getAllTasks(projectId)*/

    @GetMapping("/all")
    fun getAllComments(): Flow<Task> = taskService.getAllProjects()

}