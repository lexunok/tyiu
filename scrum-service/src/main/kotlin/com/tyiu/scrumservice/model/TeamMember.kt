package com.tyiu.scrumservice.model

import com.tyiu.ideas.model.dto.UserDTO
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate
interface TeamMemberRepository: CoroutineCrudRepository<TeamMember, String>

@Table
data class TeamMember(
    val teamId:String? = null,
    val userId:String? = null,
    var teamRole:TeamRole? = null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = LocalDate.now(),
)

data class TeamMemberDTO(
    val userId: String? = null,
    var email: UserDTO? = null,
    val firstName:UserDTO? = null,
    val lastName:UserDTO? = null,
    var teamRole:TeamRole? = null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = null,

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