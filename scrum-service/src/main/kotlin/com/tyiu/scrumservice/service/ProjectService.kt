package com.tyiu.scrumservice.service

import com.tyiu.ideas.model.toDTO
import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service

@Service
class ProjectService(
    private val projectRepository: ProjectRepository,
    private val ideaRepository: IdeaRepository,
    private val teamRepository: TeamRepository,
    private val projectMemberRepository: ProjectMemberRepository
){

    private val connections:MutableMap<String, MutableSharedFlow<Project>> = mutableMapOf()
    fun getAllProjects():Flow<ProjectDTO> =
        projectRepository.findAll().map{
            p -> val project = p.toDTO()
            /*project.name = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.description = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.customer = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.initiator = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.team = p.teamId?.let { teamRepository.findById(it) }?.toDTO()
            project.members = p.userId?.let { projectMemberRepository.findById(it) }?.toDTO()*/
            return@map project
        }


    fun getYourProjects():Flow<Project> = projectRepository.findAll()

    fun getYourActiveProjects():Flow<Project> = projectRepository.findAll()

    suspend fun getOneProject(projectId: String): ProjectDTO? {
        val prj = projectRepository.findByProjectId(projectId)?.toDTO()
        //prj?.name =
        return prj
    }

    fun getProjectMembers():Flow<Project> = projectRepository.findAll()

    fun getProjectMarks():Flow<Project> = projectRepository.findAll()

    suspend fun createProject(projectDTO: ProjectDTO): ProjectDTO {
        val project = Project(
            report = projectDTO.report,
            startDate = projectDTO.startDate,
            finishDate = projectDTO.finishDate,
            status = projectDTO.status
        )
        return projectRepository.save(project).toDTO()
    }

    fun addMembersInProject():Flow<Project> = projectRepository.findAll()

    fun putProjectMarks():Flow<Project> = projectRepository.findAll()

    fun putProjectStatus():Flow<Project> = projectRepository.findAll()

    fun putFinishProject():Flow<Project> = projectRepository.findAll()

}