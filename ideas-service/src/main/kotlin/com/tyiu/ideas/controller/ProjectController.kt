package com.tyiu.ideas.controller

import com.tyiu.client.exceptions.AccessException
import com.tyiu.client.models.Role
import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.IdeaMarketDTO
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


    @GetMapping("/all")
    fun getAllProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN))) {
            projectService.getAllProjects()
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/private/all")
    fun getYourProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.INITIATOR,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getYourProjects(jwt.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/active/all")
    fun getYourActiveProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.INITIATOR,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getYourActiveProjects(jwt.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): ProjectDTO? {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getOneProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): Flow<ProjectMemberDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getProjectMembers(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): Flow<ProjectMarksDTO> {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getProjectMarks(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO,@AuthenticationPrincipal jwt: Jwt): ProjectDTO {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN))) {
            projectService.createProject(ideaMarketDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/{projectId}/add/members")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody addToProjectRequest: AddToProjectRequest,
                                    @AuthenticationPrincipal jwt: Jwt): ProjectMemberDTO {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.addMembersInProject(projectId,addToProjectRequest)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{projectId}/status/change")
    suspend fun pauseProject(@PathVariable projectId: String, @AuthenticationPrincipal jwt: Jwt) : InfoResponse {
        return if (jwt.getClaimAsStringList("roles").roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN,Role.INITIATOR,Role.TEAM_LEADER))) {
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
        try {
            projectService.putFinishProject(
                projectId,
                report,
                jwt.id,
                jwt.getClaimAsStringList("roles").toList()
            )
            return InfoResponse(HttpStatus.OK,"Проект успешно завершён")
        }
        catch (e: AccessException) {
            throw e
        }
        catch(e: Exception){
            return InfoResponse(HttpStatus.BAD_REQUEST,"Проект не был завершён")
        }
    }
}

