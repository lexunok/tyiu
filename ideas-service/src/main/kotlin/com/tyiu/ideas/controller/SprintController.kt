package com.tyiu.ideas.controller

import com.tyiu.client.exceptions.AccessException
import com.tyiu.client.models.Role
import com.tyiu.ideas.model.*
import com.tyiu.ideas.service.SprintService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/sprint")
class SprintController(private val sprintService: SprintService)
{

    private val roles = listOf(Role.INITIATOR, Role.PROJECT_OFFICE, Role.MEMBER, Role.TEAM_LEADER, Role.TEAM_OWNER, Role.ADMIN)
    private val roles2 = listOf(Role.PROJECT_OFFICE, Role.TEAM_LEADER, Role.ADMIN)
    private val roles3 = listOf(Role.INITIATOR, Role.PROJECT_OFFICE, Role.ADMIN)

    @GetMapping("/{projectId}/all")
    fun getAllSprintsByProject(@PathVariable projectId: String, @AuthenticationPrincipal jwt: Jwt): Flow<SprintDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            sprintService.getAllSprintsByProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{id}")
    suspend fun getSprintById(@PathVariable id: String, @AuthenticationPrincipal jwt: Jwt): SprintDTO? {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            sprintService.getSprintById(id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{projectId}/active")
    suspend fun getActiveSprint(@PathVariable projectId: String, @AuthenticationPrincipal jwt: Jwt): SprintDTO? {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            sprintService.getActiveSprint(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/marks/{sprintId}/all")
    fun getAllSprintMarks(@PathVariable sprintId: String, @AuthenticationPrincipal jwt: Jwt): Flow<SprintMarkDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            sprintService.getAllSprintMarks(sprintId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/add")
    suspend fun createSprint(@RequestBody sprintDTO: SprintDTO, @AuthenticationPrincipal jwt: Jwt): SprintDTO {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles2)) {
            sprintService.createSprint(sprintDTO, jwt.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/marks/{projectId}/{sprintId}/add")
    suspend fun addSprintMarks(@PathVariable sprintId: String,
                                @PathVariable projectId: String,
                                @RequestBody sprintMarks: Flow<SprintMarkRequest>,
                                @AuthenticationPrincipal jwt: Jwt) {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles3)) {
            sprintService.addSprintMarks(sprintId, projectId, sprintMarks)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{sprintId}/update")
    suspend fun updateSprintInfo(@PathVariable sprintId: String,
                                 @RequestBody sprintDTO: SprintDTO,
                                 @AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles2)) {
            try {
                sprintService.updateSprintInfo(sprintId, sprintDTO, jwt.id)
                InfoResponse(HttpStatus.OK,"Успешное изменение спринта")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось изменить спринт")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/finish/{sprintId}")
    suspend fun finishSprint(@PathVariable sprintId: String,
                             @RequestBody report: String,
                             @AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles3)) {
            try {
                sprintService.putSprintFinish(sprintId, report, jwt.id)
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
    suspend fun deleteSprint(@PathVariable sprintId: String, @AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles2)) {
            try {
                sprintService.deleteSprint(sprintId, jwt.id)
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