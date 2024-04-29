package com.tyiu.ideas.controller

import com.tyiu.ideas.config.exception.AccessException
import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.ProjectService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/project")
class ProjectController (private val projectService: ProjectService) {

    private val member = Role.MEMBER.toString()
    private val initiator = Role.INITIATOR.toString()
    private val projectOffice = Role.PROJECT_OFFICE.toString()
    private val teamOwner = Role.TEAM_OWNER.toString()
    private val teamLeader = Role.TEAM_LEADER.toString()
    private val admin = Role.ADMIN.toString()

    private val roles = listOf(projectOffice, admin)
    private val roles2 = listOf(initiator,member,teamLeader,teamOwner,admin)
    private val roles3 = listOf(initiator,projectOffice,member,teamLeader,teamOwner,admin)
    private val roles4 = listOf(initiator,projectOffice,admin)
    private val roles5 = listOf(initiator,projectOffice,teamLeader,admin)

    @GetMapping("/all")
    fun getAllProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            projectService.getAllProjects()
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/private/all")
    fun getYourProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles2)) {
            projectService.getYourProjects(jwt.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/active/all")
    fun getYourActiveProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles2)) {
            projectService.getYourActiveProjects(jwt.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): ProjectDTO? {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles3)) {
            projectService.getOneProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): Flow<ProjectMemberDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles3)) {
            projectService.getProjectMembers(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): Flow<ProjectMarksDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles3)) {
            projectService.getProjectMarks(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO,@AuthenticationPrincipal jwt: Jwt): ProjectDTO {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles)) {
            projectService.createProject(ideaMarketDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/{projectId}/add/members")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody addToProjectRequest: AddToProjectRequest,
                                    @AuthenticationPrincipal jwt: Jwt): ProjectMemberDTO {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(teamOwner,admin,teamLeader))) {
            projectService.addMembersInProject(projectId,addToProjectRequest)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{projectId}/status/change")
    suspend fun pauseProject(@PathVariable projectId: String, @AuthenticationPrincipal jwt: Jwt) : InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles5)) {
            try {
                projectService.pauseProject(projectId)
                InfoResponse(HttpStatus.OK, "Проект успешно приостановлен")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось приостановить проект")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/finish/{projectId}")
    suspend fun putFinishProject(@PathVariable projectId: String,
                                 @RequestBody report: String,
                                 @AuthenticationPrincipal jwt: Jwt) : InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(roles4)) {
            try {
                projectService.putFinishProject(projectId, report)
                InfoResponse(HttpStatus.OK,"Проект успешно завершён")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Проект не был завершён")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    /*@PutMapping("/leader/change")
    suspend fun putTeamLeader(@RequestBody projectLeaderRequest: ProjectLeaderRequest,@AuthenticationPrincipal user: User) : InfoResponse {
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN,Role.INITIATOR,Role.TEAM_LEADER))) {
            try {
                projectService.putTeamLeader(projectLeaderRequest)
                InfoResponse(HttpStatus.OK,"Лидер команды назначен")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Лидер команды не был назначен")
            }
        }
        else {
            throw AccessException("Нет прав")
        }
    }*/

}