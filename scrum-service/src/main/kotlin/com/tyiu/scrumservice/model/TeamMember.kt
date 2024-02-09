package com.tyiu.scrumservice.model

import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate
interface TeamMemberRepository: CoroutineCrudRepository<TeamMember, String>{

    @Query("SELECT * FROM team_member WHERE team_id = :teamId")
    fun findMemberByTeamId(teamId: String): Flow<ProjectMember>
}

@Table
data class TeamMember(
    val teamId:String? = null,
    val userId:String? = null,
    var teamRole:TeamRole? = null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = LocalDate.now(),
)

data class TeamMemberDTO(
    val teamId:String? = null,
    val userId: String? = null,
    var email:String? = null,
    val firstName:String? = null,
    val lastName:String? = null,
    var teamRole:TeamRole? = null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = null,
    )

data class TeamMemberRequest(
    val userId: String? = null,
    val teamId:String? = null,
)
enum class TeamRole{
    TEAM_LEADER, MEMBER
}
fun TeamMember.toDTO(): TeamMemberDTO = TeamMemberDTO(
    userId = userId,
    teamRole = teamRole,
    startDate = startDate,
    finishDate = finishDate,
)