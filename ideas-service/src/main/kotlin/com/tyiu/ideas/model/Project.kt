package com.tyiu.ideas.model
import com.tyiu.client.models.UserDTO

import com.tyiu.ideas.model.dto.TeamDTO
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.http.HttpStatus
import java.time.LocalDate

@Table
data class Project(
    @Id
    val id:String? = null,
    val ideaId:String? = null,
    val teamId:String? = null,
    var report: String?= null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = null,
    var status: ProjectStatus? = ProjectStatus.ACTIVE,
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
        ACTIVE, DONE, PAUSED, DELETED
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