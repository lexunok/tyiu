package com.tyiu.ideas.controller

import com.tyiu.ideas.model.*
import com.tyiu.ideas.service.TaskService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/task")
class TaskController (private val taskService: TaskService) {

    @GetMapping("/projects/{projectId}/all")
    fun getAllTaskByProject(@PathVariable projectId: String): Flow<TaskDTO> = taskService.getAllTasksByProject(projectId)

    @GetMapping("/projects/{projectId}")
    fun getAllTasksInBackLog(@PathVariable projectId: String): Flow<TaskDTO> = taskService.getAllTasksInBacklog(projectId)

    @GetMapping("/projects/{projectId}/sprint/{sprintId}")
    fun getAllTasksInSprint(@PathVariable projectId: String, @PathVariable sprintId: String): Flow<TaskDTO> = taskService.getAllTasksInSprint(projectId, sprintId)

    @GetMapping("/{id}")
    fun getOneTaskById(@PathVariable id: String): Flow<TaskDTO> = taskService.getOneTaskById(id)

    @PostMapping("/add")
    suspend fun postCreateTask(@RequestBody taskDTO: TaskDTO): TaskDTO = taskService.createTask(taskDTO)

    @PutMapping("/status/change")
    suspend fun putTaskStatus (@RequestBody taskDTO: TaskDTO) = taskService.putTaskStatus(taskDTO)

    @PutMapping("/{taskId}/update")
    suspend fun putUpdateTask (@PathVariable taskId: String,@RequestBody taskDTO: TaskDTO) = taskService.putUpdateTask(taskId, taskDTO)

    @PutMapping("/{taskId}/{executorId}")
    suspend fun putUpdateExecutorTask(@PathVariable taskId: String, @PathVariable executor: String, @RequestBody taskDTO: TaskDTO) = taskService.putUpdateExecutorTask(taskId, executor, taskDTO)

    @DeleteMapping("/{id}/delete")
    suspend fun deleteTask(@PathVariable id: String) = taskService.deleteTask(id)
}