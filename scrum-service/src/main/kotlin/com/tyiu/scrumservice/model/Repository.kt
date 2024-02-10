package com.tyiu.scrumservice.model

import com.tyiu.ideas.model.dto.IdeaDTO
import com.tyiu.ideas.model.dto.TeamDTO
import com.tyiu.ideas.model.dto.UserDTO
import com.tyiu.ideas.model.entities.Idea
import com.tyiu.ideas.model.entities.Team
import com.tyiu.ideas.model.entities.User
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface IdeaRepository: CoroutineCrudRepository<Idea, String>
interface TeamRepository: CoroutineCrudRepository<Team, String>

interface UserRepository: CoroutineCrudRepository<User, String>
fun Idea.toDTO(): IdeaDTO = IdeaDTO(
    id,
    name,
    description,
    customer,
    initiatorId,
)

fun Team.toDTO(): TeamDTO = TeamDTO(
    id,
    name,
)

fun User.toDTO(): UserDTO = UserDTO(
    id,
    email,
    firstName,
    lastName,
)