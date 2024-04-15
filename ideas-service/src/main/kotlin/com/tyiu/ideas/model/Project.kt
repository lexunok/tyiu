package com.tyiu.ideas.model

import com.tyiu.ideas.model.dto.TeamDTO
import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import org.springframework.http.HttpStatus
import java.time.LocalDate

interface ProjectRepository: CoroutineCrudRepository<Project, String>
@Table
data class Project(
        @Id
        val id:String? = null,
        val ideaId:String? = null,
        val teamId:String? = null,
        val report: String?= null,
        val startDate:LocalDate? = LocalDate.now(),
        val finishDate:LocalDate? = null,
        val status: ProjectStatus? = ProjectStatus.ACTIVE,
)

data class ProjectDTO (
        var id:String? = null,
        val ideaId:String? = null,
        var name: String?=null,
        var description: String?=null,
        var customer: String?=null,
        var initiator: UserDTO? = null,
        var team: TeamDTO? = null,
        var members: List<ProjectMemberDTO>? = null,
        var report: ReportProject? = null,
        val startDate:LocalDate?,
        val finishDate:LocalDate?,
        var status: ProjectStatus?,
)

enum class ProjectStatus{
        ACTIVE, DONE, PAUSED
}

data class InfoResponse(
        val statusCode: HttpStatus? = null,
        val message: String? = null,
)

data class ReportProject(
        val projectId:String? = null,
        var marks:List<ProjectMarksDTO>? = null,
        val report:String? = null,
)

fun Project.toDTO(): ProjectDTO=ProjectDTO(
        id = id,
        ideaId = ideaId,
        startDate = startDate,
        finishDate = finishDate,
        status = status,
)