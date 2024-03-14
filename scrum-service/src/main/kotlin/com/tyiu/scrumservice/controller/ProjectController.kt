package com.tyiu.scrumservice.controller

import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.ideas.model.entities.User
import com.tyiu.ideas.model.enums.Role
import com.tyiu.scrumservice.model.*
import com.tyiu.scrumservice.service.ProjectService
import kotlinx.coroutines.flow.Flow
import org.springframework.http.HttpStatus
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/project")
class ProjectController (private val projectService: ProjectService) {

    @GetMapping("/all")
    fun getAllProjects(@AuthenticationPrincipal user: User): Flow<ProjectDTO>? {
        return if (user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)){
            projectService.getAllProjects()}
        else {null}
    }

    @GetMapping("/private/all")
    fun getYourProjects(@AuthenticationPrincipal user: User): Flow<ProjectDTO>? {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)) {
            projectService.getYourProjects(user.id)}
        else {null}
    }

    @GetMapping("/active/all")
    fun getYourActiveProjects(@AuthenticationPrincipal user: User): Flow<ProjectDTO>? {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)) {
            projectService.getYourActiveProjects(user.id)}
        else {null}
    }

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: String,@AuthenticationPrincipal user: User): ProjectDTO? {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)) {
            projectService.getOneProject(projectId)}
        else {null}
    }

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String,@AuthenticationPrincipal user: User): Flow<ProjectMemberDTO>? {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)) {
            projectService.getProjectMembers(projectId)}
        else {null}
    }

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String,@AuthenticationPrincipal user: User): Flow<ProjectMarksDTO>? {
        return if (user.roles.contains(Role.MEMBER)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.TEAM_OWNER)
            ||user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)) {
            projectService.getProjectMarks(projectId)}
        else {null}
    }

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO,@AuthenticationPrincipal user: User): Flow<ProjectMember>? {
        return if (user.roles.contains(Role.ADMIN)||user.roles.contains(Role.PROJECT_OFFICE)){
            projectService.createProject(ideaMarketDTO)}
        else {null}
    }

    @PostMapping("/{projectId}/add/members")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody teamMemberRequest: TeamMemberRequest,
                                    @AuthenticationPrincipal user: User): ProjectMember? {
        return if (user.roles.contains(Role.ADMIN)||user.roles.contains(Role.TEAM_OWNER)){
            projectService.addMembersInProject(projectId,teamMemberRequest)}
        else {null}
    }

    @PostMapping("/{projectId}/add/marks")
    suspend fun addMarksInProject(@PathVariable projectId: String,@RequestBody projectMarksRequest: projectMarksRequest,
                                  @AuthenticationPrincipal user: User): ProjectMarks?{
        return if (user.roles.contains(Role.ADMIN)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.PROJECT_OFFICE)){
            projectService.addMarksInProject(projectId, projectMarksRequest)}
        else {null}
    }

    @PutMapping("/{projectId}/status/change")
    suspend fun pauseProject(@PathVariable projectId: String, @AuthenticationPrincipal user: User) : InfoResponse {
        return if (user.roles.contains(Role.ADMIN)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.PROJECT_OFFICE))
        { try {
            projectService.pauseProject(projectId)
            InfoResponse(HttpStatus.OK, "Статус проекта изменён")
        }
        catch(e: Exception){
            InfoResponse(HttpStatus.BAD_REQUEST, "Статус проекта не был изменён")
        } }
        else {InfoResponse(HttpStatus.BAD_REQUEST, "Статус проекта не был изменён")}
    }

    @PutMapping("/{projectId}/finish/change")
    suspend fun putFinishProject(@PathVariable projectId: String,@RequestBody projectFinishRequest: ProjectFinishRequest,
                                 @AuthenticationPrincipal user: User) : InfoResponse {
        return if (user.roles.contains(Role.ADMIN)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.PROJECT_OFFICE))
        { try {
            projectService.putFinishProject(projectId,projectFinishRequest)
            InfoResponse(HttpStatus.OK,"Проект успешно завершён")
        }
        catch(e: Exception){
            InfoResponse(HttpStatus.BAD_REQUEST,"Проект не был завершён")
        } }
        else {InfoResponse(HttpStatus.BAD_REQUEST,"Проект не был завершён")}
    }

    @PutMapping("/leader/change")
    suspend fun putTeamLeader(@RequestBody projectLeaderRequest: ProjectLeaderRequest,@AuthenticationPrincipal user: User) : InfoResponse {
        return if (user.roles.contains(Role.ADMIN)||user.roles.contains(Role.INITIATOR)||user.roles.contains(Role.PROJECT_OFFICE))
        { try {
            projectService.putTeamLeader(projectLeaderRequest)
            InfoResponse(HttpStatus.OK,"Лидер команды назначён")
        }
        catch(e: Exception){
            InfoResponse(HttpStatus.BAD_REQUEST,"Лидер команды не был назначен")
        } }
        else {InfoResponse(HttpStatus.BAD_REQUEST,"Лидер команды не был назначен")}
    }

}