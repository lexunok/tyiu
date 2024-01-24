package com.tyiu.scrumservice.model

import com.tyiu.scrumservice.model.dto.UserDTO
import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository
import java.time.LocalDate

interface ProjectMemberRepository: CoroutineCrudRepository<ProjectMember, String>

@Table
data class ProjectMember (
    val projectId:String?=null,
    val userId:String?=null,
    val teamId:String?=null,
    val projectRole: String?=null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = null
)

data class ProjectMemberDTO(
    val userId:String?=null,
    val teamId:String?=null,
    var email: UserDTO? = null,
    val firstName:UserDTO? = null,
    val lastName:UserDTO? = null,
    val projectRole:String?=null,
    val startDate:LocalDate? = LocalDate.now(),
    val finishDate:LocalDate? = LocalDate.now(),
)

fun ProjectMember.toDTO():ProjectMemberDTO = ProjectMemberDTO(
    userId = userId,
    teamId = teamId,
    projectRole = projectRole,
    startDate = startDate,
    finishDate = finishDate,
)
