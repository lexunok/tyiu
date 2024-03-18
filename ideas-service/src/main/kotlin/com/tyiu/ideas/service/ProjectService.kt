package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.IdeaMarketDTO
import kotlinx.coroutines.flow.*
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service

@Service
class ProjectService(
    private val teamMemberRepository: TeamMemberRepository,
    private val projectRepository: ProjectRepository,
    private val ideaRepository: IdeaRepository,
    private val teamRepository: TeamRepository,
    private val projectMemberRepository: ProjectMemberRepository,
    private val projectMarksRepository: ProjectMarksRepository,
    val template: R2dbcEntityTemplate,
    private val userRepository: UserRepository,
    private val taskRepository: TaskRepository,
    private val taskService: TaskService,
    private val marketRepository: MarketRepository
) {
    private suspend fun projectToDTO(project: Project): ProjectDTO {
        val projects = project.toDTO()
        val ideaToProject = project.ideaId?.let { ideaRepository.findById(it) }
        projects.name = ideaToProject?.name
        projects.description = ideaToProject?.description
        projects.customer = ideaToProject?.customer
        projects.initiator = ideaToProject?.initiatorId?.let { userRepository.findById(it)?.toDTO()}
        projects.team = project.teamId?.let { teamRepository.findById(it) }?.toDTO()
        projects.members = project.id?.let{ getProjectMembers(it) }?.toList()
        projects.report = ReportProject(project.id,project.id?.let { getProjectMarks(it).toList() },project.report)
        return projects
    }

    fun getAllProjects(): Flow<ProjectDTO> = projectRepository.findAll().map { projectToDTO(it) }

    fun getYourProjects(userId: String): Flow<ProjectDTO> = projectRepository.findProjectByUserId(userId).map { projectToDTO(it) }

    fun getYourActiveProjects(userId: String): Flow<ProjectDTO> = projectRepository.findByStatus(userId).map { projectToDTO(it) }

    suspend fun getOneProject(projectId: String): ProjectDTO? = projectRepository.findById(projectId)?.let { projectToDTO(it) }

    fun getProjectMembers(projectId: String): Flow<ProjectMemberDTO> =
        projectMemberRepository.findMemberByProjectId(projectId).map { p ->
            val projectMember = p.toDTO()
            val userToProject = p.userId?.let { userRepository.findById(it) }?.toDTO()
            projectMember.email = userToProject?.email
            projectMember.firstName = userToProject?.firstName
            projectMember.lastName = userToProject?.lastName
            return@map projectMember
        }

    fun getProjectMarks(projectId: String): Flow<ProjectMarksDTO> =
        projectMarksRepository.findMarksByProjectId(projectId).map{ m ->
            val projectMarks = m.toDTO()
            val userToProject = m.userId?.let { userRepository.findById(it) }?.toDTO()
            projectMarks.firstName = userToProject?.firstName
            projectMarks.lastName = userToProject?.lastName
            projectMarks.tasks = m.userId?.let{taskRepository.findTaskByExecutorId(it).map{taskService.taskToDTO(it)}}?.toList()
            return@map projectMarks
        }

    suspend fun createProject(ideaMarketDTO: IdeaMarketDTO): ProjectDTO {
        val project = Project(
            ideaId = ideaMarketDTO.ideaId,
            teamId = ideaMarketDTO.team.id,
            finishDate = ideaMarketDTO.marketId?.let {marketRepository.findById(it) }?.finishDate

        )
        val createdProject = projectRepository.save(project)
        teamMemberRepository.findMemberByTeamId(ideaMarketDTO.team.id).toList().forEach { m ->
            projectMemberRepository.save(
                ProjectMember(
                    projectId = createdProject.id,
                    userId = m.userId,
                    teamId = m.teamId
                )
            )
        }
        return createdProject.toDTO()
    }

    suspend fun addMembersInProject(projectId: String, teamMemberRequest: TeamMemberRequest): ProjectMemberDTO {
        val projectMember = ProjectMember(
            projectId = projectId,
            userId = teamMemberRequest.userId,
            teamId = teamMemberRequest.teamId
        )
        return projectMemberRepository.save(projectMember).toDTO()
    }

    suspend fun addMarksInProject(projectId: String, projectMarkRequest: projectMarksRequest): ProjectMarksDTO{
        val projectMarks = ProjectMarks(
            projectId = projectId,
            userId = projectMarkRequest.userId,
            mark = projectMarkRequest.mark
        )
        return projectMarksRepository.save(projectMarks).toDTO()
    }

    suspend fun pauseProject(projectId: String) {
        val query = "UPDATE project SET status = 'PAUSED' WHERE id = :projectId"
        return template.databaseClient.sql(query).bind("projectId", projectId).await()
    }

    suspend fun putFinishProject(projectId: String,projectFinishRequest:ProjectFinishRequest) {
        val query = "UPDATE project SET finish_date = :finishDate, report = :projectReport, status = 'DONE' WHERE id = :projectId"
        return template.databaseClient.sql(query)
            .bind("finishDate", projectFinishRequest.finishDate!!)
            .bind("projectReport", projectFinishRequest.projectReport!!)
            .bind("projectId", projectId).await()
    }

    suspend fun putTeamLeader(projectLeaderRequest:ProjectLeaderRequest){
        val query = "UPDATE project_member SET project_role = 'TEAM_LEADER' WHERE user_id = :userId and project_id =:projectId"
        return template.databaseClient.sql(query)
            .bind("userId", projectLeaderRequest.userId!!)
            .bind("projectId",projectLeaderRequest.projectId!!).await()
    }
}



