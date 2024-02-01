package com.tyiu.scrumservice.controller

import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.scrumservice.model.*
import com.tyiu.scrumservice.service.ProjectService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*
import java.math.BigInteger
import java.time.LocalDate

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
    suspend fun getOneProject(@PathVariable projectId: BigInteger): Flow<ProjectDTO> = projectService.getOneProject(projectId)

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String): Flow<ProjectMemberDTO> = projectService.getProjectMembers(projectId)

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String): Flow<ProjectMarks> = projectService.getProjectMarks(projectId)

    @GetMapping("/taskMovementLog/{projectId}/all")
    fun getProjectLogs(@PathVariable projectId: String): Flow<TaskMovementLog> = projectService.getProjectLogs(projectId)

    @PostMapping("/send")
    suspend fun createProject(@RequestBody ideaMarketDTO: IdeaMarketDTO): Flow<Project> = projectService.createProject(ideaMarketDTO)
    @PostMapping("/add/members")
    fun addMembersInProject(): Flow<Project> = projectService.addMembersInProject()

    @PutMapping("/marks/{projectId}/update")
    fun putProjectMarks(@PathVariable projectId: BigInteger): Flow<Project> = projectService.putProjectMarks()

    @PutMapping("/status/change/{projectId}")
    fun putProjectStatus(@PathVariable projectId: BigInteger,@RequestBody projectStatus: ProjectStatus): Flow<ProjectDTO> = projectService.putProjectStatus(projectId,projectStatus)

    @PutMapping("/finish/change")
    fun putFinishProject(@RequestBody projectDTO: ProjectDTO): Flow<Project> = projectService.putFinishProject(projectDTO)

}