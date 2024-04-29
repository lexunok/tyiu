package com.tyiu.ideas.controller

import com.tyiu.ideas.config.exception.AccessException
import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.TaskService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/task")
class TaskController (private val taskService: TaskService) {

    private val member = Role.MEMBER.toString()
    private val initiator = Role.INITIATOR.toString()
    private val projectOffice = Role.PROJECT_OFFICE.toString()
    private val teamOwner = Role.TEAM_OWNER.toString()
    private val teamLeader = Role.TEAM_LEADER.toString()
    private val admin = Role.ADMIN.toString()

    private val roles = listOf(initiator, projectOffice, member, teamOwner, teamLeader, admin)

    @GetMapping("/project/all/{projectId}")
    fun getAllTaskByProject(@PathVariable projectId: String, @AuthenticationPrincipal jwt: Jwt): Flow<TaskDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            taskService.getAllTasksByProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/project/backlog/{projectId}")
    fun getAllTasksInBackLog(@PathVariable projectId: String, @AuthenticationPrincipal jwt: Jwt): Flow<TaskDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            taskService.getAllTasksInBacklog(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/project/sprint/{sprintId}")
    fun getAllTasksInSprint(@PathVariable sprintId: String, @AuthenticationPrincipal jwt: Jwt): Flow<TaskDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            taskService.getAllTasksInSprint(sprintId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{taskId}")
    suspend fun getOneTaskById(@PathVariable taskId: String, @AuthenticationPrincipal jwt: Jwt): TaskDTO? {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            taskService.getOneTaskById(taskId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add")
    suspend fun postCreateTask(@RequestBody taskDTO: TaskDTO, @AuthenticationPrincipal jwt: Jwt): TaskDTO {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            taskService.createTask(taskDTO, jwt.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/update/{taskId}")
    suspend fun putUpdateTask (@PathVariable taskId: String,
                               @RequestBody taskDTO: TaskDTO,
                               @AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
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
                                      @AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
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

    @PutMapping("/comment/{taskId}")
    suspend fun updateLeaderCommentInTask(@PathVariable taskId: String, @RequestBody taskDTO: TaskDTO){
        taskService.updateLeaderCommentInTask(taskId, taskDTO.leaderComment!!)
    }

    @PutMapping("/description/{taskId}")
    suspend fun updateDescriptionInTask(@PathVariable taskId: String, @RequestBody taskDTO: TaskDTO){
        taskService.updateDescriptionInTask(taskId, taskDTO.description!!)
    }

    @PutMapping("/name/{taskId}")
    suspend fun updateNameInTask(@PathVariable taskId: String, @RequestBody taskDTO: TaskDTO){
        taskService.updateNameInTask(taskId, taskDTO.name!!)
    }

    @DeleteMapping("/delete/{taskId}")
    suspend fun deleteTask(@PathVariable taskId: String, @AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
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