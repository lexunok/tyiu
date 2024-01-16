package com.tyiu.scrumservice.service

import com.tyiu.scrumservice.model.Project
import com.tyiu.scrumservice.model.ProjectDTO
import com.tyiu.scrumservice.model.ProjectRepository
import kotlinx.coroutines.flow.Flow
import org.springframework.stereotype.Service

@Service
class ProjectService(private val repository: ProjectRepository){

    fun getAllProjects():Flow<Project> = repository.findAll()

}