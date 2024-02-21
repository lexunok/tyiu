package com.tyiu.scrumservice.controller


import com.tyiu.scrumservice.model.*
import com.tyiu.scrumservice.service.TaskMovementLogService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/tasklog")
class TaskMovementLogController (private val TaskMovementLogService: TaskMovementLogService) {
    @GetMapping("/{taskId}")
    fun getAllTaskByTaskId(@PathVariable taskId: String): Flow<TaskMovementLogDTO> = TaskMovementLogService.getAllTaskLog(taskId)
}