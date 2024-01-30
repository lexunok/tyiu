package com.tyiu.scrumservice.model

import com.tyiu.ideas.model.dto.IdeaDTO
import com.tyiu.ideas.model.dto.TeamDTO
import com.tyiu.ideas.model.dto.UserDTO
import com.tyiu.ideas.model.entities.Idea
import com.tyiu.ideas.model.entities.Team
import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.math.BigInteger
import java.time.LocalDate

interface IdeaRepository: CoroutineCrudRepository<Idea, String> {}

interface TeamRepository: CoroutineCrudRepository<Team, String> {}
interface ProjectRepository: CoroutineCrudRepository<Project, String>{
        @Query("SELECT * FROM project WHERE id = :projectId")
        fun findByProjectId(projectId: BigInteger): Flow<Project>

        @Query("SELECT * FROM project WHERE status = 'ACTIVE'")
        fun findByStatus(): Flow<Project>
}
@Table
data class Project(
        @Id
        val id:String? = null,
        val ideaId:String? = null,
        val teamId:String? = null,
        val report:String? = null,
        val startDate:LocalDate? = LocalDate.now(),
        val finishDate:LocalDate? = null,
        val status: ProjectStatus? = ProjectStatus.ACTIVE,
)

data class ProjectDTO (
        val id:String? = null,
        var name: IdeaDTO? = null,
        var description:IdeaDTO? = null,
        var customer:IdeaDTO? = null,
        var initiator: IdeaDTO? = null,
        var team: TeamDTO? = null,
        var members: List<ProjectMemberDTO>? = null,
        val report: String? = null,
        val startDate:LocalDate? = LocalDate.now(),
        val finishDate:LocalDate? = LocalDate.now(),
        var status: ProjectStatus? = ProjectStatus.ACTIVE,
)

enum class ProjectStatus{
        ACTIVE, DONE, FAILED
}

fun Project.toDTO(): ProjectDTO=ProjectDTO(
        id = id,
        report = report,
        startDate = startDate,
        finishDate = finishDate,
        status = status,
)

fun ProjectDTO.toEntity(): Project = Project(
        report = report,
        startDate = startDate,
        finishDate = finishDate,
        status = status,
)

/*fun Idea.toDTO(): IdeaDTO = IdeaDTO(
        id,
        name,
        description,
        customer,
        initiatorId
)

fun Team.toDTO(): TeamDTO = TeamDTO(
        id,
        name,
)*/