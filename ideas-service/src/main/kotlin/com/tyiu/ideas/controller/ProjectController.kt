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

    private val PROJECT_OFFICE = Role.ADMIN.toString()
    private val ADMIN = Role.ADMIN.toString()
    private val INITIATOR = Role.INITIATOR.toString()
    private val MEMBER = Role.MEMBER.toString()
    private val TEAM_OWNER = Role.TEAM_OWNER.toString()
    private val TEAM_LEADER = Role.TEAM_LEADER.toString()

    @GetMapping("/all")
    fun getAllProjects(@AuthenticationPrincipal user: Jwt): Flow<ProjectDTO> {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(PROJECT_OFFICE, ADMIN))) {
            projectService.getAllProjects()
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/private/all")
    fun getYourProjects(@AuthenticationPrincipal user: Jwt): Flow<ProjectDTO> {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(INITIATOR,MEMBER,TEAM_OWNER,ADMIN,TEAM_LEADER))) {
            projectService.getYourProjects(user.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/active/all")
    fun getYourActiveProjects(@AuthenticationPrincipal user: Jwt): Flow<ProjectDTO> {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(INITIATOR,MEMBER,TEAM_OWNER,ADMIN,TEAM_LEADER))) {
            projectService.getYourActiveProjects(user.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: String,@AuthenticationPrincipal user: Jwt): ProjectDTO? {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(INITIATOR,PROJECT_OFFICE,MEMBER,TEAM_OWNER,ADMIN,TEAM_LEADER))) {
            projectService.getOneProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String,@AuthenticationPrincipal user: Jwt): Flow<ProjectMemberDTO> {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(INITIATOR,PROJECT_OFFICE,MEMBER,TEAM_OWNER,ADMIN,TEAM_LEADER))) {
            projectService.getProjectMembers(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String,@AuthenticationPrincipal user: Jwt): Flow<ProjectMarksDTO> {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(INITIATOR,PROJECT_OFFICE,MEMBER,TEAM_OWNER,ADMIN,TEAM_LEADER))) {
            projectService.getProjectMarks(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO,@AuthenticationPrincipal user: Jwt): ProjectDTO {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(PROJECT_OFFICE,ADMIN))) {
            projectService.createProject(ideaMarketDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/{projectId}/add/members")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody addToProjectRequest: AddToProjectRequest,
                                    @AuthenticationPrincipal user: Jwt): ProjectMemberDTO {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(TEAM_OWNER,ADMIN,TEAM_LEADER))) {
            projectService.addMembersInProject(projectId,addToProjectRequest)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{projectId}/add/marks")
    suspend fun addMarksInProject(@PathVariable projectId: String, @AuthenticationPrincipal user: Jwt) {

        return if (user.getClaimAsStringList("roles").roleCheck(listOf(INITIATOR,PROJECT_OFFICE,ADMIN))) {
            projectService.addMarksInProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{projectId}/status/change")
    suspend fun pauseProject(@PathVariable projectId: String, @AuthenticationPrincipal user: Jwt) : InfoResponse {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(PROJECT_OFFICE,ADMIN,INITIATOR,TEAM_LEADER))) {
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
                                 @AuthenticationPrincipal user: Jwt) : InfoResponse {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(PROJECT_OFFICE,ADMIN,INITIATOR))) {
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

    @PutMapping("/leader/change")
    suspend fun putTeamLeader(@RequestBody projectLeaderRequest: ProjectLeaderRequest,@AuthenticationPrincipal user: Jwt) : InfoResponse {
        return if (user.getClaimAsStringList("roles").roleCheck(listOf(PROJECT_OFFICE,ADMIN,INITIATOR,TEAM_LEADER))) {
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
    }

}