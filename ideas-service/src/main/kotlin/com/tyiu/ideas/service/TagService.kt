package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.reactive.asFlow
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import org.springframework.data.relational.core.query.Update.update
import org.springframework.stereotype.Service

@Service
class TagService (val template: R2dbcEntityTemplate) {

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    fun getAllTags(): Flow<TagDTO> {
        return template.select(Tag::class.java).all().asFlow().map { it.toDTO() }
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    suspend fun createTag(tagDTO: TagDTO, userId: String): TagDTO {
        return template.insert(
            Tag(
                name = tagDTO.name,
                color = tagDTO.color,
                confirmed = true,
                creatorId = userId
            )
        ).awaitSingle().toDTO()
    }

    suspend fun createNoConfirmedTag(tagDTO: TagDTO, userId: String): TagDTO {
        return template.insert(
            Tag(
                name = tagDTO.name,
                color = tagDTO.color,
                confirmed = false,
                creatorId = userId
            )
        ).awaitSingle().toDTO()
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    suspend fun confirmTag(userId: String, tagId: String) {
        template.update(query(where("id").`is`(tagId)),
            update("updater_id", userId)
                .set("confirmed", true),
            Tag::class.java).awaitSingle()
    }

    suspend fun updateTag(tagDTO: TagDTO, userId: String, tagId: String) {
        template.update(query(where("id").`is`(tagId)),
            update("updater_id", userId)
                .set("name", tagDTO.name)
                .set("color",tagDTO.color),
            Tag::class.java).awaitSingle()
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    suspend fun deleteTag(tagId: String) {
        template.delete(query(where("id").`is`(tagId)), Tag::class.java).awaitSingle()
    }
}