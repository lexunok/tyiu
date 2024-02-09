package com.tyiu.scrumservice.service

import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.ideas.model.toDTO
import com.tyiu.scrumservice.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.toList
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
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
    val template: R2dbcEntityTemplate,
    private val userRepository: UserRepository
) {
    //suspend везде но не там где GET
    //не делать много запросов в бд, лучше получить один раз модель и вставить ее в дто
    //не делать много преобразований из дто и в дто
    //return map можно убрать
    //статус Paused  запрос чтобы его поменять
    //отдельная модель
    //по названию функции в репозитории формируется запрос автоматически
    suspend fun projectToDTO(project: Project): ProjectDTO {
        val projects = project.toDTO()
        projects.name = project.ideaId?.let { ideaRepository.findById(it) }?.toDTO()?.name
        projects.description = project.ideaId?.let { ideaRepository.findById(it) }?.toDTO()?.description
        projects.customer = project.ideaId?.let { ideaRepository.findById(it) }?.toDTO()?.customer
        projects.initiator = project.ideaId?.let { userRepository.findById(it) }?.toDTO()
        projects.team = project.teamId?.let { teamRepository.findById(it) }?.toDTO()
        projects.members = getProjectMembers(project.id.toString()).toList()
        return projects
    }

    fun getAllProjects(): Flow<ProjectDTO> =
        projectRepository.findAll().map { p ->
            return@map projectToDTO(p)
        }

    fun getYourProjects(userId: String): Flow<ProjectDTO> =
        projectRepository.findProjectByUserId(userId).map { projectToDTO(it) }

    fun getYourActiveProjects(userId: String): Flow<ProjectDTO> =
        projectRepository.findByStatus(userId).map { p ->
            return@map projectToDTO(p)
        }

    fun getOneProject(projectId: BigInteger): Flow<ProjectDTO> =
        projectRepository.findByProjectId(projectId).map { p ->
            return@map projectToDTO(p)
        }

    fun getProjectMembers(projectId: String): Flow<ProjectMemberDTO> =
        projectMemberRepository.findMemberByProjectId(projectId).map { p ->
            val projectMember = p.toDTO()
            projectMember.email = p.userId?.let { userRepository.findById(it) }?.toDTO()?.email
            projectMember.firstName = p.userId?.let { userRepository.findById(it) }?.toDTO()?.firstName
            projectMember.lastName = p.userId?.let { userRepository.findById(it) }?.toDTO()?.lastName
            return@map projectMember
        }

    fun getProjectMarks(projectId: String): Flow<ProjectMarks> =
        projectMarksRepository.findMarksByProjectId(projectId)

    fun getProjectLogs(projectId: String): Flow<TaskMovementLog> =
        taskMovementLogRepository.findLogByProjectId(projectId)

    suspend fun createProject(ideaMarketDTO: IdeaMarketDTO): ProjectDTO {
        val project = Project(
            ideaId = ideaMarketDTO.ideaId
            //teamId = ideaMarketDTO.
        )
        return projectToDTO(projectRepository.save(project))
    }

    suspend fun addMembersInProject(projectId: String, teamMemberRequest: TeamMemberRequest): ProjectMemberDTO {
        val projectMember = ProjectMember(
            projectId = projectId,
            userId = teamMemberRequest.userId,
            teamId = teamMemberRequest.teamId
        )
        val projectMemberToDTO = projectMemberRepository.save(projectMember).toDTO()
        projectMemberToDTO.email = projectMember.userId?.let { userRepository.findById(it) }?.toDTO()?.email
        projectMemberToDTO.firstName = projectMember.userId?.let { userRepository.findById(it) }?.toDTO()?.firstName
        projectMemberToDTO.lastName = projectMember.userId?.let { userRepository.findById(it) }?.toDTO()?.lastName
        return projectMemberToDTO
    }

    suspend fun putProjectMarks(projectMarks: ProjectMarks) {
        val query = "UPDATE project_marks SET mark = :mark WHERE user_id = :userId"
        return template.databaseClient.sql(query)
            .bind("mark", projectMarks.mark!!)
            .bind("userId", projectMarks.userId!!).await()
    }

    suspend fun putProjectStatus(projectStatusRequest: ProjectStatusRequest) {
        val query = "UPDATE project SET status = :projectStatus WHERE id = :projectId"
        return template.databaseClient.sql(query)
            .bind("projectStatus", projectStatusRequest.projectStatus.toString())
            .bind("projectId", projectStatusRequest.projectId!!).await()
    }

    suspend fun putFinishProject(projectFinishRequest: ProjectFinishRequest) {
        val query = "UPDATE project SET finish_date = :finishDate, report = :projectReport WHERE id = :projectId"
        return template.databaseClient.sql(query)
            .bind("finishDate", projectFinishRequest.finishDate!!)
            .bind("projectReport", projectFinishRequest.projectReport!!)
            .bind("projectId", projectFinishRequest.projectId!!).await()
    }
}



