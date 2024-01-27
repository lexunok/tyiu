package com.tyiu.scrumservice.controller

import com.tyiu.scrumservice.model.Task
import com.tyiu.scrumservice.service.TaskService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*
@RestController
@RequestMapping("/api/v1/project")
class TaskController (private val projectService: TaskService) {

    @GetMapping("/all")
    fun getAllComments(): Flow<Task> = projectService.getAllProjects()

}