package com.tyiu.scrumservice.service

import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service
import java.math.BigInteger

@Service
class ProjectService(
    private val projectRepository: ProjectRepository,
    private val ideaRepository: IdeaRepository,
    private val teamRepository: TeamRepository,
    private val projectMemberRepository: ProjectMemberRepository,
    private val projectMarksRepository: ProjectMarksRepository
) {

    private val connections: MutableMap<String, MutableSharedFlow<Project>> = mutableMapOf()
    fun getAllProjects(): Flow<ProjectDTO> =
        projectRepository.findAll().map { p ->
            val project = p.toDTO()
            /*project.name = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.description = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.customer = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.initiator = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.team = p.teamId?.let { teamRepository.findById(it) }?.toDTO()
            project.members = p.userId?.let { projectMemberRepository.findById(it) }?.toDTO()*/
            return@map project
        }


    fun getYourProjects(): Flow<ProjectDTO> = //привязывается к ProjectMembers пока не готово
        projectRepository.findAll().map { p ->
            val project = p.toDTO()
            /*project.name = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.description = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.customer = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.initiator = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.team = p.teamId?.let { teamRepository.findById(it) }?.toDTO()
            project.members = p.userId?.let { projectMemberRepository.findById(it) }?.toDTO()*/
            return@map project
        }

    fun getYourActiveProjects(): Flow<ProjectDTO> = //привязывается к ProjectMembers пока не готово но выводит все ACTIVE
        projectRepository.findByStatus().map {
            p -> val project = p.toDTO()
            /*project.name = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.description = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.customer = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.initiator = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.team = p.teamId?.let { teamRepository.findById(it) }?.toDTO()
            project.members = p.userId?.let { projectMemberRepository.findById(it) }?.toDTO()*/
            return@map project
        }

    suspend fun getOneProject(projectId: BigInteger): Flow<ProjectDTO> =
        projectRepository.findByProjectId(projectId).map {
            p -> val project =p.toDTO()
            /*project.name = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.description = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.customer = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.initiator = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
            project.team = p.teamId?.let { teamRepository.findById(it) }?.toDTO()
            project.members = p.userId?.let { projectMemberRepository.findById(it) }?.toDTO()*/
            return@map project
        }


    fun getProjectMembers(projectId: String): Flow<ProjectMemberDTO> =
        projectMemberRepository.findMemberByProjectId(projectId).map {
            p -> val projectMember = p.toDTO()
            /*projectMember.email = p.userId?.let { ideaRepository.findById(it) }?.toDTO()
            projectMember.firstName = p.userId?.let { ideaRepository.findById(it) }?.toDTO()
            projectMember.lastName = p.userId?.let { ideaRepository.findById(it) }?.toDTO()*/
            return@map projectMember
        }

    fun getProjectMarks(projectId: String): Flow<ProjectMarks> = projectMarksRepository.findMarksByProjectId(projectId)

    suspend fun createProject(projectDTO: ProjectDTO): ProjectDTO {
        val project = Project(
            report = projectDTO.report,
            startDate = projectDTO.startDate,
            finishDate = projectDTO.finishDate,
            status = projectDTO.status
        )
        return projectRepository.save(project).toDTO()
    }

    fun addMembersInProject(): Flow<Project> = projectRepository.findAll()

    fun putProjectMarks(): Flow<Project> = projectRepository.findAll()

    fun putProjectStatus(): Flow<Project> = projectRepository.findAll()

    fun putFinishProject(): Flow<Project> = projectRepository.findAll()
}

