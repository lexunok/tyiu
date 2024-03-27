package com.tyiu.ideas.model

import com.tyiu.ideas.model.dto.IdeaDTO
import com.tyiu.ideas.model.dto.MarketDTO
import com.tyiu.ideas.model.dto.TeamDTO
import com.tyiu.ideas.model.entities.Idea
import com.tyiu.ideas.model.entities.Market
import com.tyiu.ideas.model.entities.Team
import com.tyiu.ideas.model.entities.relations.Team2Member
import kotlinx.coroutines.flow.Flow
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

interface IdeaRepository: CoroutineCrudRepository<Idea, String>
interface TeamRepository: CoroutineCrudRepository<Team, String>
interface MarketRepository: CoroutineCrudRepository<Market, String>
interface TeamToMemberRepository: CoroutineCrudRepository<Team2Member, String>{
    @Query("SELECT * FROM team_member WHERE team_id =:teamId")
    fun findMembersByTeamId(teamId: String): Flow<Team2Member>

    @Query("SELECT COUNT(*) FROM team_member WHERE team_id = :teamId AND finish_date IS NULL")
    fun countTeam2MemberByTeamId(teamId: String): Flow<Int>
}

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