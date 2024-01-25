package com.tyiu.scrumservice.model


import com.tyiu.ideas.model.dto.IdeaDTO
import com.tyiu.ideas.model.dto.TeamDTO
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface ProjectRepository: CoroutineCrudRepository<Project, String>{

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
        val name: IdeaDTO? = null,
        val description:IdeaDTO? = null,
        val customer:IdeaDTO? = null,
        var initiator: IdeaDTO? = null,
        var team: TeamDTO? = null,
        var members: List<String>? = null,
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
