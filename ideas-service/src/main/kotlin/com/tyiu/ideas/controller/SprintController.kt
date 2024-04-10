package com.tyiu.ideas.controller

import com.tyiu.ideas.config.exception.AccessException
import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.enums.Role
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
    private val member = Role.MEMBER.toString()
    private val initiator = Role.INITIATOR.toString()
    private val projectOffice = Role.PROJECT_OFFICE.toString()
    private val teamOwner = Role.TEAM_OWNER.toString()
    private val teamLeader = Role.TEAM_LEADER.toString()
    private val admin = Role.ADMIN.toString()

    private val roles = listOf(initiator, projectOffice, admin, teamLeader, teamOwner, member)
    private val roles2 = listOf(projectOffice, teamLeader, member)
    private val roles3 = listOf(initiator, projectOffice, member)

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
            sprintService.createSprint(sprintDTO, jwt)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/{sprintId}/add/marks")
    suspend fun  addSprintMarks(@PathVariable sprintId: String,
                                @RequestBody sprintMarks: Flow<SprintMarkDTO>,
                                @AuthenticationPrincipal jwt: Jwt) {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles3)) {
            sprintService.addSprintMarks(sprintId, sprintMarks)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{sprintId}/status/{status}")
    suspend fun changeSprintStatus(@PathVariable sprintId: String, @PathVariable status: SprintStatus, @AuthenticationPrincipal jwt: Jwt): InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles2)) {
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
                                 @AuthenticationPrincipal jwt: Jwt) {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles2)) {
            sprintService.updateSprintInfo(sprintId, sprintDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/finish/{sprintId}")
    suspend fun putSprintFinishWithoutTaskTransfer(@PathVariable sprintId: String,
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