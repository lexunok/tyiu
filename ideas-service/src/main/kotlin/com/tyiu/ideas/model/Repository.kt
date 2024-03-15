package com.tyiu.ideas.model

import com.tyiu.ideas.model.dto.IdeaDTO
import com.tyiu.ideas.model.dto.TeamDTO
import com.tyiu.ideas.model.entities.Idea
import com.tyiu.ideas.model.entities.Team
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface IdeaRepository: CoroutineCrudRepository<Idea, String>
interface TeamRepository: CoroutineCrudRepository<Team, String>
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