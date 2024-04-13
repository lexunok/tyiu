package com.tyiu.ideas.controller

import com.tyiu.ideas.config.exception.AccessException
import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.ProjectService
import com.tyiu.ideas.util.roleCheck
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/project")
class ProjectController (private val projectService: ProjectService) {

    @GetMapping("/all")
    fun getAllProjects(@AuthenticationPrincipal user: User): Flow<ProjectDTO> {
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN))) {
            projectService.getAllProjects()
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/private/all")
    fun getYourProjects(@AuthenticationPrincipal user: User): Flow<ProjectDTO> {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getYourProjects(user.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/active/all")
    fun getYourActiveProjects(@AuthenticationPrincipal user: User): Flow<ProjectDTO> {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getYourActiveProjects(user.id)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: String,@AuthenticationPrincipal user: User): ProjectDTO? {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getOneProject(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String,@AuthenticationPrincipal user: User): Flow<ProjectMemberDTO> {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getProjectMembers(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String,@AuthenticationPrincipal user: User): Flow<ProjectMarksDTO> {
        return if (user.roles.roleCheck(listOf(Role.INITIATOR,Role.PROJECT_OFFICE,Role.MEMBER,Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.getProjectMarks(projectId)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO,@AuthenticationPrincipal user: User): ProjectDTO {
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN))) {
            projectService.createProject(ideaMarketDTO)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PostMapping("/{projectId}/add/members")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody addToProjectRequest: AddToProjectRequest,
                                    @AuthenticationPrincipal user: User): ProjectMemberDTO {
        return if (user.roles.roleCheck(listOf(Role.TEAM_OWNER,Role.ADMIN,Role.TEAM_LEADER))) {
            projectService.addMembersInProject(projectId,addToProjectRequest)
        }
        else {
            throw AccessException("Нет прав")
        }
    }

    @PutMapping("/{projectId}/status/change")
    suspend fun pauseProject(@PathVariable projectId: String, @AuthenticationPrincipal user: User) : InfoResponse {
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN,Role.INITIATOR,Role.TEAM_LEADER))) {
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
                                 @AuthenticationPrincipal user: User) : InfoResponse {
        return if (user.roles.roleCheck(listOf(Role.PROJECT_OFFICE,Role.ADMIN,Role.INITIATOR))) {
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
    }

}