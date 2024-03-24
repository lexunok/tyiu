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

    @GetMapping("/{projectId}/all")
    fun getAllSprintsByProject(@PathVariable projectId: String, @AuthenticationPrincipal user: User): Flow<SprintDTO> {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            sprintService.getAllSprintsByProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{id}")
    suspend fun getSprintById(@PathVariable id: String, @AuthenticationPrincipal user: User): SprintDTO? {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            sprintService.getSprintById(id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{projectId}/active")
    suspend fun getActiveSprint(@PathVariable projectId: String, @AuthenticationPrincipal user: User): SprintDTO {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            sprintService.getActiveSprint(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/marks/{sprintId}/all")
    fun getAllSprintMarks(@PathVariable sprintId: String, @AuthenticationPrincipal user: User): Flow<SprintMarks> {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN))) {
            sprintService.getAllSprintMarks(sprintId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add")
    suspend fun createSprint(@RequestBody sprintDTO: SprintDTO, @AuthenticationPrincipal user: User): SprintDTO {
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN))) {
            sprintService.createSprint(sprintDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/{sprintId}/add/marks")
    suspend fun  addSprintMarks(@PathVariable sprintId: String,
                                @RequestBody sprintMarksRequest: SprintMarksRequest,
                                @AuthenticationPrincipal user: User): SprintMarks {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.ADMIN))) {
            sprintService.addSprintMarks(sprintId, sprintMarksRequest)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{sprintId}/status/{status}")
    suspend fun changeSprintStatus(@PathVariable sprintId: String, @PathVariable status: SprintStatus, @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN))) {
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
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN))) {
            sprintService.updateSprintInfo(sprintId, sprintDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{sprintId}/finish")
    suspend fun putSprintFinish(@PathVariable sprintId: String,
                                @RequestBody sprintFinishRequest: SprintFinishRequest,
                                @AuthenticationPrincipal user: User): InfoResponse {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.ADMIN))) {
            try {
                sprintService.putSprintFinish(sprintId, sprintFinishRequest)
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
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN))) {
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