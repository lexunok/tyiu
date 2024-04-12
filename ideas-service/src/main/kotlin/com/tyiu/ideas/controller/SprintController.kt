package com.tyiu.ideas.controller

import com.tyiu.ideas.config.exception.AccessException
import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.SprintService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/sprint")
class SprintController(private val sprintService: SprintService)
{

    private val roles = listOf(Role.INITIATOR, Role.PROJECT_OFFICE, Role.MEMBER, Role.TEAM_LEADER, Role.TEAM_OWNER, Role.ADMIN)
    private val roles2 = listOf(Role.PROJECT_OFFICE, Role.TEAM_LEADER, Role.ADMIN)
    private val roles3 = listOf(Role.INITIATOR, Role.PROJECT_OFFICE, Role.ADMIN)

    @GetMapping("/{projectId}/all")
    fun getAllSprintsByProject(@PathVariable projectId: String, @AuthenticationPrincipal user: User): Flow<SprintDTO> {
        return if (user.roles.roleCheck(roles)) {
            sprintService.getAllSprintsByProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{id}")
    suspend fun getSprintById(@PathVariable id: String, @AuthenticationPrincipal user: User): SprintDTO? {
        return if (user.roles.roleCheck(roles)) {
            sprintService.getSprintById(id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{projectId}/active")
    suspend fun getActiveSprint(@PathVariable projectId: String, @AuthenticationPrincipal user: User): SprintDTO? {
        return if (user.roles.roleCheck(roles)) {
            sprintService.getActiveSprint(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/marks/{sprintId}/all")
    fun getAllSprintMarks(@PathVariable sprintId: String, @AuthenticationPrincipal user: User): Flow<SprintMarkDTO> {
        return if (user.roles.roleCheck(roles)) {
            sprintService.getAllSprintMarks(sprintId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add")
    suspend fun createSprint(@RequestBody sprintDTO: SprintDTO, @AuthenticationPrincipal user: User): SprintDTO {
        return if (user.roles.roleCheck(roles2)) {
            sprintService.createSprint(sprintDTO, user)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/marks/{projectId}/{sprintId}/add")
    suspend fun  addSprintMarks(@PathVariable sprintId: String,
                                @PathVariable projectId: String,
                                @RequestBody sprintMarks: Flow<SprintMarkDTO>,
                                @AuthenticationPrincipal user: User) {
        return if (user.roles.roleCheck(roles3)) {
            sprintService.addSprintMarks(sprintId, projectId, sprintMarks)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{sprintId}/status/{status}")
    suspend fun changeSprintStatus(@PathVariable sprintId: String, @PathVariable status: SprintStatus, @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(roles2)) {
            try {
                sprintService.changeSprintStatus(sprintId, status)
                InfoResponse(HttpStatus.OK, "Статус спринта успешно изменён на $status")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось изменить статус спринта")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{sprintId}/update")
    suspend fun updateSprintInfo(@PathVariable sprintId: String,
                                 @RequestBody sprintDTO: SprintDTO,
                                 @AuthenticationPrincipal user: User) {
        return if (user.roles.roleCheck(roles2)) {
            sprintService.updateSprintInfo(sprintId, sprintDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/finish/{sprintId}")
    suspend fun putSprintFinishWithoutTaskTransfer(@PathVariable sprintId: String,
                                                   @RequestBody report: String,
                                                   @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(roles3)) {
            try {
                sprintService.putSprintFinish(sprintId, report, user.id)
                InfoResponse(HttpStatus.OK,"Спринт успешно завершён")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при завершении спринта")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @DeleteMapping("/{sprintId}/delete")
    suspend fun deleteSprint(@PathVariable sprintId: String, @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(roles2)) {
            try {
                sprintService.deleteSprint(sprintId)
                InfoResponse(HttpStatus.OK,"Спринт успешно удалён")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при удалении спринта")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

}