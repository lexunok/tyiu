package com.tyiu.scrumservice.controller

import com.tyiu.scrumservice.model.Project
import com.tyiu.scrumservice.model.ProjectDTO
import com.tyiu.scrumservice.service.ProjectService
import kotlinx.coroutines.flow.Flow
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/v1/project")
class ProjectController (private val projectService: ProjectService) {

    @GetMapping("/all")
    fun getAllComments(): Flow<Project> = projectService.getAllProjects()

}