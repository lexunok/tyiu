package com.tyiu.scrumservice.model

import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface ProjectMemberRepository: CoroutineCrudRepository<ProjectMember, String>{
    @Query("SELECT * FROM project_member WHERE project_id = :projectId")
    fun findMemberByProjectId(projectId: String): Flow<ProjectMember>

    @Query("SELECT * FROM project_member WHERE user_id = :userId")
    fun findProjectByUserId(userId: String): Flow<Project>

}


@Table
data class ProjectMember (
    val projectId:String?=null,
    val userId:String?=null,
    val teamId:String?=null,
    val projectRole: ProjectRole?=null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = null
)

data class ProjectMemberDTO(
    val userId:String?=null,
    val teamId:String?=null,
    var email: UserDTO? = null,
    var firstName:UserDTO? = null,
    var lastName:UserDTO? = null,
    val projectRole:ProjectRole?=null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = LocalDate.now(),
)

enum class ProjectRole{
    TEAM_LEADER, INITIATOR, MEMBER
}

fun ProjectMember.toDTO():ProjectMemberDTO = ProjectMemberDTO(
    userId = userId,
    teamId = teamId,
    projectRole = projectRole,
    startDate = startDate,
    finishDate = finishDate,
)
