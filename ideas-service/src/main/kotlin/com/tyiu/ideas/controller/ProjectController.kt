package com.tyiu.ideas.controller

import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.ideas.model.enums.Role
import com.tyiu.ideas.service.ProjectService
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/project")
class ProjectController (private val projectService: ProjectService) {

    private fun roleCheck(jwt: Jwt): Boolean {
        return (jwt.getClaimAsStringList("roles").contains(Role.MEMBER.toString())
                || jwt.getClaimAsStringList("roles").contains(Role.INITIATOR.toString())
                || jwt.getClaimAsStringList("roles").contains(Role.TEAM_OWNER.toString())
                || jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
                || jwt.getClaimAsStringList("roles").contains(Role.PROJECT_OFFICE.toString()))
    }

    @GetMapping("/all")
    fun getAllProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO>? {
        return if (jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.PROJECT_OFFICE.toString()))
        {
            projectService.getAllProjects()
        }
        else {null}
    }

    @GetMapping("/private/all")
    fun getYourProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO>? {
        return if (jwt.getClaimAsStringList("roles").contains(Role.MEMBER.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.INITIATOR.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.TEAM_OWNER.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString()))
        {
            projectService.getYourProjects(jwt.id)
        }
        else {null}
    }

    @GetMapping("/active/all")
    fun getYourActiveProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO>? {
        return if (jwt.getClaimAsStringList("roles").contains(Role.MEMBER.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.INITIATOR.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.TEAM_OWNER.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString()))
        {
            projectService.getYourActiveProjects(jwt.id)
        }
        else {null}
    }

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): ProjectDTO? {
        return if (roleCheck(jwt))
        {
            projectService.getOneProject(projectId)
        }
        else {null}
    }

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): Flow<ProjectMemberDTO>? {
        return if (roleCheck(jwt))
        {
            projectService.getProjectMembers(projectId)
        }
        else {null}
    }

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String,@AuthenticationPrincipal jwt: Jwt): Flow<ProjectMarksDTO>? {
        return if (roleCheck(jwt))
        {
            projectService.getProjectMarks(projectId)
        }
        else {null}
    }

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO,@AuthenticationPrincipal jwt: Jwt): Project? {
        return if (jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.PROJECT_OFFICE.toString()))
        {
            projectService.createProject(ideaMarketDTO)
        }
        else {null}
    }

    @PostMapping("/{projectId}/add/members")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody teamMemberRequest: TeamMemberRequest,
                                    @AuthenticationPrincipal jwt: Jwt): ProjectMember? {
        return if (jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.TEAM_OWNER.toString()))
        {
            projectService.addMembersInProject(projectId,teamMemberRequest)
        }
        else {null}
    }

    @PostMapping("/{projectId}/add/marks")
    suspend fun addMarksInProject(@PathVariable projectId: String,@RequestBody projectMarksRequest: projectMarksRequest,
                                  @AuthenticationPrincipal jwt: Jwt): ProjectMarks?{
        return if (jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.INITIATOR.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.PROJECT_OFFICE.toString()))
        {
            projectService.addMarksInProject(projectId, projectMarksRequest)
        }
        else {null}
    }

    @PutMapping("/{projectId}/status/change")
    suspend fun pauseProject(@PathVariable projectId: String, @AuthenticationPrincipal jwt: Jwt) : InfoResponse {
        return if (jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.INITIATOR.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.PROJECT_OFFICE.toString()))
        {
            try {
                projectService.pauseProject(projectId)
                InfoResponse(HttpStatus.OK, "Статус проекта изменён")
            } catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST, "Статус проекта не был изменён")
            }
        }
        else {InfoResponse(HttpStatus.BAD_REQUEST, "Статус проекта не был изменён")}
    }

    @PutMapping("/{projectId}/finish/change")
    suspend fun putFinishProject(@PathVariable projectId: String,@RequestBody projectFinishRequest: ProjectFinishRequest,
                                 @AuthenticationPrincipal jwt: Jwt) : InfoResponse {
        return if (jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.INITIATOR.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.PROJECT_OFFICE.toString()))
        {
            try {
                projectService.putFinishProject(projectId,projectFinishRequest)
                InfoResponse(HttpStatus.OK,"Проект успешно завершён")
            } catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Проект не был завершён")
            }
        }
        else {InfoResponse(HttpStatus.BAD_REQUEST,"Проект не был завершён")}
    }

    @PutMapping("/leader/change")
    suspend fun putTeamLeader(@RequestBody projectLeaderRequest: ProjectLeaderRequest,@AuthenticationPrincipal jwt: Jwt) : InfoResponse {
        return if (jwt.getClaimAsStringList("roles").contains(Role.ADMIN.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.INITIATOR.toString())
            || jwt.getClaimAsStringList("roles").contains(Role.PROJECT_OFFICE.toString()))
        {
            try {
                projectService.putTeamLeader(projectLeaderRequest)
                InfoResponse(HttpStatus.OK,"Лидер команды назначен")
            }
            catch(e: Exception){
                InfoResponse(HttpStatus.BAD_REQUEST,"Лидер команды не был назначен")
            }
        }
        else {InfoResponse(HttpStatus.BAD_REQUEST,"Лидер команды не был назначен")}
    }

}