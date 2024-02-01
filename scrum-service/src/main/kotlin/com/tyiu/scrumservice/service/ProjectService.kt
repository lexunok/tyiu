package com.tyiu.scrumservice.service

import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.map
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.r2dbc.core.bind
import org.springframework.stereotype.Service
import java.math.BigInteger

@Service
class ProjectService(
    private val projectRepository: ProjectRepository,
    private val ideaRepository: IdeaRepository,
    private val teamRepository: TeamRepository,
    private val projectMemberRepository: ProjectMemberRepository,
    private val projectMarksRepository: ProjectMarksRepository,
    private val taskMovementLogRepository: TaskMovementLogRepository,
    val template: R2dbcEntityTemplate
) {

    private val connections: MutableMap<String, MutableSharedFlow<Project>> = mutableMapOf()
    suspend fun projectToDTO(project: Project): ProjectDTO {
    val projects = project.toDTO()
    /*projects.name = project.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
    projects.description = project.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
    projects.customer = project.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
    projects.initiator = project.ideaId?.let { ideaRepository.findById(it) }?.toDTO()
    projects.team = project.teamId?.let { teamRepository.findById(it) }?.toDTO()
    projects.members = listOf(ProjectMemberDTO(userI ))*/
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

    suspend fun putProjectStatus(projectStatusRequest: ProjectStatusRequest) {
        val query = "UPDATE project SET status = :projectStatus WHERE id = :projectId"
        return template.databaseClient.sql(query)
            .bind("projectStatus", projectStatusRequest.projectStatus.toString())
            .bind("projectId", projectStatusRequest.projectId!!).await()
    }

    suspend fun putFinishProject(projectFinishDateRequest: ProjectFinishDateRequest){
        val query = "UPDATE project SET finish_date = :finishDate WHERE id = :projectId"
        return template.databaseClient.sql(query)
            .bind("finishDate", projectFinishDateRequest.finishDate!!)
            .bind("projectId", projectFinishDateRequest.projectId!!).await()
    }
}

