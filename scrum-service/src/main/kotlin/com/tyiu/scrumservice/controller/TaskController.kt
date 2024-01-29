package com.tyiu.scrumservice.controller

import com.tyiu.ideas.model.CommentDTO
import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.model.toDTO
import com.tyiu.scrumservice.model.Task
import com.tyiu.scrumservice.model.TaskDTO
import com.tyiu.scrumservice.service.TaskService
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.web.bind.annotation.*
import java.math.BigInteger

@RestController
@RequestMapping("/api/v1/scrum-service/task")
class TaskController (private val taskService: TaskService  ) {

    @GetMapping("projects/{projectId}/all")
    fun getAllTaskByProject(@PathVariable projectId: String): Flow<TaskDTO> = taskService.getAllTasksByProject(projectId)

    @GetMapping("projects/{projectId}")
    fun getAllTasksInBackLog(@PathVariable projectId: String): Flow<TaskDTO> = taskService.getAllTasksInBacklog(projectId)

    @GetMapping("projects/{projectId}/sprint/{sprintId}") // ПОКА ЧТО НЕ РАБОТАЕТ.
    fun getAllTasksInSprint(@PathVariable projectId: String, sprintId: String): Flow<TaskDTO> = taskService.getAllTasksInSprint(projectId, sprintId)

    @GetMapping("{id}")
    fun getOneTaskById(@PathVariable id: BigInteger): Flow<TaskDTO> = taskService.getOneTaskById(id)

    @GetMapping("/all")  // ВЫВОД ВСЕХ СОЗДАННЫХ ТАСКОВ ВО ВСЕХ ПРОЕКТАХ. НУЖНО НА ВРЕМЯ РАЗРАБОТКИ
    fun getAllTasks(): Flow<Task> = taskService.getAllTasks()

    /*@GetMapping("{status}")
    fun getAllTasksInBackLog(@PathVariable status: String): Flow<TaskDTO> = taskService.getAllTasksInBacklog(status)
    ПОИСК ТАСКА ПО ТЕГУ может пригодится в будущем
    */

    @DeleteMapping("{id}/delete")
    suspend fun deleteTask(@PathVariable id: BigInteger) = taskService.deleteTask(id)
}