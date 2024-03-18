package com.tyiu.ideas.model

import com.tyiu.ideas.model.dto.IdeaDTO
import com.tyiu.ideas.model.dto.MarketDTO
import com.tyiu.ideas.model.dto.TeamDTO
import com.tyiu.ideas.model.entities.Idea
import com.tyiu.ideas.model.entities.Market
import com.tyiu.ideas.model.entities.Team
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface IdeaRepository: CoroutineCrudRepository<Idea, String>
interface TeamRepository: CoroutineCrudRepository<Team, String>
interface MarketRepository: CoroutineCrudRepository<Market, String>

fun Market.toDTO(): MarketDTO = MarketDTO(
    id,
    finishDate
)

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