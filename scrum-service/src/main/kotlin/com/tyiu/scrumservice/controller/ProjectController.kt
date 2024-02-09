package com.tyiu.scrumservice.controller

import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.scrumservice.model.*
import com.tyiu.scrumservice.service.ProjectService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*
import java.math.BigInteger

@RestController
@RequestMapping("/api/v1/scrum-service/project")
class ProjectController (private val projectService: ProjectService) {

    @GetMapping("/all")
    fun getAllProjects(): Flow<ProjectDTO> = projectService.getAllProjects()

    @GetMapping("/{userId}/private/all")
    fun getYourProjects(@PathVariable userId: String): Flow<ProjectDTO> = projectService.getYourProjects(userId)

    @GetMapping("/{userId}/active/all")
    fun getYourActiveProjects(@PathVariable userId: String): Flow<ProjectDTO> = projectService.getYourActiveProjects(userId)

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: String): ProjectDTO? = projectService.getOneProject(projectId)

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String): Flow<ProjectMemberDTO>? = projectService.getProjectMembers(projectId)

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String): Flow<ProjectMarks> = projectService.getProjectMarks(projectId)

    @GetMapping("/taskMovementLog/{projectId}/all")
    fun getProjectLogs(@PathVariable projectId: String): Flow<TaskMovementLog> = projectService.getProjectLogs(projectId)

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO): ProjectDTO = projectService.createProject(ideaMarketDTO)

    @PostMapping("/{projectId}/add/members")
    suspend fun addMembersInProject(@PathVariable projectId: String, @RequestBody teamMemberRequest: TeamMemberRequest): ProjectMemberDTO = projectService.addMembersInProject(projectId,teamMemberRequest)

    @PutMapping("/marks/update")
    suspend fun putProjectMarks(@RequestBody projectMarks: ProjectMarks) = projectService.putProjectMarks(projectMarks)


    @PutMapping("/{projectId}/status/change")
    suspend fun pauseProject(@PathVariable projectId: String)= projectService.pauseProject(projectId)

    @PutMapping("/{projectId}/finish/change")
    suspend fun putFinishProject(@PathVariable projectId: String,@RequestBody projectFinishRequest: ProjectFinishRequest)= projectService.putFinishProject(projectId,projectFinishRequest)

}