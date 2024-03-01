package com.tyiu.scrumservice.controller

import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.scrumservice.model.*
import com.tyiu.scrumservice.service.ProjectService
import kotlinx.coroutines.flow.Flow
import org.springframework.security.core.annotation.AuthenticationPrincipal
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/scrum-service/project")
class ProjectController (private val projectService: ProjectService) {

    @GetMapping("/all")
    fun getAllProjects(): Flow<ProjectDTO> = projectService.getAllProjects()

    @GetMapping("/private/all")
    fun getYourProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO> = projectService.getYourProjects(jwt.id)

    @GetMapping("/active/all")
    fun getYourActiveProjects(@AuthenticationPrincipal jwt: Jwt): Flow<ProjectDTO> = projectService.getYourActiveProjects(jwt.id)

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: String): ProjectDTO? = projectService.getOneProject(projectId)

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String): Flow<ProjectMemberDTO>? = projectService.getProjectMembers(projectId)

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String): Flow<ProjectMarksDTO>? = projectService.getProjectMarks(projectId)

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO): Flow<ProjectMember> = projectService.createProject(ideaMarketDTO)

    @PostMapping("/{projectId}/add/members")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody teamMemberRequest: TeamMemberRequest): ProjectMember= projectService.addMembersInProject(projectId,teamMemberRequest)

    @PostMapping("/{projectId}/add/marks")
    suspend fun addMarksInProject(@PathVariable projectId: String,@RequestBody projectMarksRequest: projectMarksRequest) = projectService.addMarksInProject(projectId, projectMarksRequest)

    @PutMapping("/{projectId}/status/change")
    suspend fun pauseProject(@PathVariable projectId: String)= projectService.pauseProject(projectId)

    @PutMapping("/{projectId}/finish/change")
    suspend fun putFinishProject(@PathVariable projectId: String,@RequestBody projectFinishRequest: ProjectFinishRequest)= projectService.putFinishProject(projectId,projectFinishRequest)

    @PutMapping("/leader/change")
    suspend fun putTeamLeader(@RequestBody projectLeaderRequest: ProjectLeaderRequest)= projectService.putTeamLeader(projectLeaderRequest)

}