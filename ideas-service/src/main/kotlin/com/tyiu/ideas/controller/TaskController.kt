package com.tyiu.ideas.controller

import com.tyiu.client.exceptions.AccessException
import com.tyiu.client.models.Role
import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.service.TaskService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/task")
class TaskController (private val taskService: TaskService) {

    private val roles = listOf(Role.INITIATOR, Role.PROJECT_OFFICE, Role.MEMBER, Role.TEAM_OWNER, Role.TEAM_LEADER, Role.ADMIN)

    @GetMapping("/project/all/{projectId}")
    fun getAllTaskByProject(@PathVariable projectId: String, @AuthenticationPrincipal user: User): Flow<TaskDTO> {
        return if (user.roles.roleCheck(roles)) {
            taskService.getAllTasksByProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/project/backlog/{projectId}")
    fun getAllTasksInBackLog(@PathVariable projectId: String, @AuthenticationPrincipal user: User): Flow<TaskDTO> {
        return if (user.roles.roleCheck(roles)) {
            taskService.getAllTasksInBacklog(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/project/sprint/{sprintId}")
    fun getAllTasksInSprint(@PathVariable sprintId: String, @AuthenticationPrincipal user: User): Flow<TaskDTO> {
        return if (user.roles.roleCheck(roles)) {
            taskService.getAllTasksInSprint(sprintId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{taskId}")
    suspend fun getOneTaskById(@PathVariable taskId: String, @AuthenticationPrincipal user: User): TaskDTO? {
        return if (user.roles.roleCheck(roles)) {
            taskService.getOneTaskById(taskId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add")
    suspend fun postCreateTask(@RequestBody taskDTO: TaskDTO, @AuthenticationPrincipal user: User): TaskDTO {
        return if (user.roles.roleCheck(roles)) {
            taskService.createTask(taskDTO, user.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/update/{taskId}")
    suspend fun putUpdateTask (@PathVariable taskId: String,
                               @RequestBody taskDTO: TaskDTO,
                               @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(roles)) {
            try {
                taskService.putUpdateTask(taskId, taskDTO)
                InfoResponse(HttpStatus.OK,"Задача успешно изменена")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось изменить задачу")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/executor/{taskId}/{executorId}")
    suspend fun putUpdateExecutorTask(@PathVariable taskId: String,
                                      @PathVariable executorId: String,
                                      @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(roles)) {
            try {
                taskService.putUpdateExecutorTask(taskId, executorId)
                InfoResponse(HttpStatus.OK,"Новый исполнитель успешно назначен")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось назначить исполнителя")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/move/{taskId}/{position}")
    suspend fun moveTask(@PathVariable taskId: String, @PathVariable position: Int){
        taskService.changePosition(taskId, position)
    }

    @PutMapping("/leader/comment/{taskId}")
    suspend fun updateLeaderCommentInTask(@PathVariable taskId: String, @RequestBody leaderComment: String){
        taskService.updateLeaderCommentInTask(taskId, leaderComment)
    }

    @PutMapping("/executor/comment/{taskId}")
    suspend fun updateExecutorCommentInTask(@PathVariable taskId: String, @RequestBody executorComment: String){
        taskService.updateExecutorCommentInTask(taskId, executorComment)
    }

    @DeleteMapping("/delete/{taskId}")
    suspend fun deleteTask(@PathVariable taskId: String, @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(roles)) {
            try {
                taskService.deleteTask(taskId)
                InfoResponse(HttpStatus.OK,"Задача успешно удалена")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при удалении задачи")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }
}