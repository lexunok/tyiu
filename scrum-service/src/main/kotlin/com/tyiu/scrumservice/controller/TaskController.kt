package com.tyiu.scrumservice.controller

import com.tyiu.scrumservice.model.Task
import com.tyiu.scrumservice.service.TaskService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*
@RestController
@RequestMapping("/api/v1/task")
class TaskController (private val taskService: TaskService  ) {

    @GetMapping("/all")
    fun getAllComments(): Flow<Task> = taskService.getAllProjects()

}