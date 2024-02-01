package com.tyiu.scrumservice.service

import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service
import java.math.BigInteger
import java.time.LocalDate

@Service
class ProjectService(
    private val projectRepository: ProjectRepository,
    private val ideaRepository: IdeaRepository,
    private val teamRepository: TeamRepository,
    private val projectMemberRepository: ProjectMemberRepository,
    private val projectMarksRepository: ProjectMarksRepository,
    private val taskMovementLogRepository: TaskMovementLogRepository,
) {

    private val connections: MutableMap<String, MutableSharedFlow<Project>> = mutableMapOf()
    fun projectToDTO(project: Project): ProjectDTO {
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
            p -> return@map projectToDTO(p)
        }


    fun getYourProjects(userId: String): Flow<ProjectDTO> = //привязывается к ProjectMembers
        projectRepository.findProjectByUserId(userId).map {
            p -> return@map projectToDTO(p)
        }

    fun getYourActiveProjects(userId: String): Flow<ProjectDTO> = //привязывается к ProjectMembers пока не готово но выводит все ACTIVE
        projectRepository.findByStatus(userId).map {
            p -> return@map projectToDTO(p)
        }

    suspend fun getOneProject(projectId: BigInteger): Flow<ProjectDTO> =
        projectRepository.findByProjectId(projectId).map {
                p -> return@map projectToDTO(p)
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

    fun getProjectLogs(projectId: String): Flow<TaskMovementLog> = taskMovementLogRepository.findLogByProjectId(projectId)

    suspend fun createProject(ideaMarketDTO: IdeaMarketDTO): Flow<Project> = projectRepository.findAll()

    fun addMembersInProject(): Flow<Project> = projectRepository.findAll()

    fun putProjectMarks(): Flow<Project> = projectRepository.findAll()

    fun putProjectStatus(projectId: BigInteger, projectStatus: ProjectStatus): Flow<ProjectDTO> =
        projectRepository.updateProjectStatus(projectId, projectStatus).map {
                p -> return@map projectToDTO(p)
        }

    fun putFinishProject(projectDTO: ProjectDTO): Flow<Project> = projectRepository.findAll()
}

