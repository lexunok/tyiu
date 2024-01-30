package com.tyiu.scrumservice.controller

import com.tyiu.ideas.model.entities.User
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

    @GetMapping("/private/all")
    fun getYourProjects(): Flow<ProjectDTO> = projectService.getYourProjects()

    @GetMapping("/active/all")
    fun getYourActiveProjects(): Flow<ProjectDTO> = projectService.getYourActiveProjects()

    @GetMapping("/{projectId}")
    suspend fun getOneProject(@PathVariable projectId: BigInteger): Flow<ProjectDTO> = projectService.getOneProject(projectId)

    @GetMapping("/members/{projectId}/all")
    fun getProjectMembers(@PathVariable projectId: String): Flow<ProjectMemberDTO> = projectService.getProjectMembers(projectId)

    @GetMapping("/marks/{projectId}/all")
    fun getProjectMarks(@PathVariable projectId: String): Flow<ProjectMarks> = projectService.getProjectMarks(projectId)

    @PostMapping("/send")
    suspend fun createProject(@RequestBody project: ProjectDTO): ProjectDTO = projectService.createProject(project)
    @PostMapping("/add/members")
    fun addMembersInProject(): Flow<Project> = projectService.addMembersInProject()

    @PutMapping("/marks/{projectId}/update")
    fun putProjectMarks(@PathVariable projectId: String): Flow<Project> = projectService.putProjectMarks()

    @PutMapping("/status/change/{projectStatus}")
    fun putProjectStatus(@PathVariable projectStatus: String): Flow<Project> = projectService.putProjectStatus()

    @PutMapping("/finish/change")
    fun putFinishProject(): Flow<Project> = projectService.putFinishProject()

}