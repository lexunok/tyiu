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
    fun ProjectToDTO(project: Project): ProjectDTO {
    val projects = project.toDTO()
    /*projects.name = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
    projects.description = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
    projects.customer = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
    projects.initiator = p.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
    projects.team = p.teamId?.let { teamRepository.findById(it) }?.toDTO()
    projects.members = p.userId?.let { projectMemberRepository.findById(it) }?.toDTO()*/
        return projects
}



    fun getAllProjects(): Flow<ProjectDTO> =
        projectRepository.findAll().map {
            p -> return@map ProjectToDTO(p)
        }


    fun getYourProjects(userId: String): Flow<ProjectDTO> = //привязывается к ProjectMembers
        projectMemberRepository.findProjectByUserId(userId).map {
            p -> return@map ProjectToDTO(p)
        }

    fun getYourActiveProjects(): Flow<ProjectDTO> = //привязывается к ProjectMembers пока не готово но выводит все ACTIVE
        projectRepository.findByStatus().map {
            p -> return@map ProjectToDTO(p)
        }

    suspend fun getOneProject(projectId: BigInteger): Flow<ProjectDTO> =
        projectRepository.findByProjectId(projectId).map {
            p -> return@map ProjectToDTO(p)
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
        val project = projectDTO.toEntity()
        return projectRepository.save(project).toDTO()
    }

    fun addMembersInProject(): Flow<Project> = projectRepository.findAll()

    fun putProjectMarks(): Flow<Project> = projectRepository.findAll()

    fun putProjectStatus(): Flow<Project> = projectRepository.findAll()

    fun putFinishProject(): Flow<Project> = projectRepository.findAll()
}

