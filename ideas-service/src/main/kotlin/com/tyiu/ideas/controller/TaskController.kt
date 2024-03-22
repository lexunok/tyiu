package com.tyiu.ideas.controller

import com.tyiu.ideas.model.*
import com.tyiu.ideas.service.TaskService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/task")
class TaskController (private val taskService: TaskService) {

    @GetMapping("/project/all/{projectId}")
    fun getAllTaskByProject(@PathVariable projectId: String): Flow<TaskDTO> = taskService.getAllTasksByProject(projectId)

    @GetMapping("/project/backlog/{projectId}")
    fun getAllTasksInBackLog(@PathVariable projectId: String): Flow<TaskDTO> = taskService.getAllTasksInBacklog(projectId)

    @GetMapping("/project/sprint/{sprintId}")
    fun getAllTasksInSprint(@PathVariable sprintId: String): Flow<TaskDTO> = taskService.getAllTasksInSprint(sprintId)

    @GetMapping("/{taskId}")
    fun getOneTaskById(@PathVariable taskId: String): Flow<TaskDTO> = taskService.getOneTaskById(taskId)

    @PostMapping("/add")
    suspend fun postCreateTask(@RequestBody taskDTO: TaskDTO): TaskDTO = taskService.createTask(taskDTO)

    @PutMapping("/status/change/{taskId}")
    suspend fun putTaskStatus (@PathVariable taskId:String, @RequestBody taskDTO: TaskDTO) = taskService.putTaskStatus(taskDTO)

    @PutMapping("/update/{taskId}")
    suspend fun putUpdateTask (@PathVariable taskId: String,@RequestBody taskDTO: TaskDTO) = taskService.putUpdateTask(taskId, taskDTO)

    @PutMapping("/executor/{taskId}/{executorId}")
    suspend fun putUpdateExecutorTask(@PathVariable taskId: String, @PathVariable executorId: String) = taskService.putUpdateExecutorTask(taskId, executorId)

    @DeleteMapping("/delete/{id}")
    suspend fun deleteTask(@PathVariable id: String) = taskService.deleteTask(id)
}