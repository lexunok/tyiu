package com.tyiu.ideas.model

import org.springframework.data.relational.core.mapping.Table
import java.time.LocalDate

@Table
data class ProjectMember (
    val projectId:String?=null,
    var userId:String?=null,
    val teamId:String?=null,
    var projectRole: ProjectRole?=ProjectRole.MEMBER,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = null
)

data class ProjectMemberDTO(
    val userId:String?=null,
    val teamId:String?=null,
    var email:String? = null,
    var firstName:String? = null,
    var lastName:String? = null,
    var projectRole:ProjectRole?,
    val startDate:LocalDate?,
    val finishDate:LocalDate?,
)

data class AddToProjectRequest(
    val teamId:String?=null,
    val userId:String?=null,
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
