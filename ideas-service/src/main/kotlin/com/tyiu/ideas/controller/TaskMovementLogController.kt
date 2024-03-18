package com.tyiu.ideas.controller


import com.tyiu.ideas.model.TaskMovementLogDTO
import com.tyiu.ideas.service.TaskMovementLogService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/log")
class TaskMovementLogController (private val taskMovementLogService: TaskMovementLogService) {
    @GetMapping("/{taskId}")
    fun getAllTaskByTaskId(@PathVariable taskId: String): Flow<TaskMovementLogDTO> = taskMovementLogService.getAllTaskLog(taskId)
}