package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import io.r2dbc.spi.Row
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.reactive.asFlow
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Criteria
import org.springframework.data.relational.core.query.Query
import org.springframework.data.relational.core.query.Update
import org.springframework.stereotype.Service

@Service
class TagService (val template: R2dbcEntityTemplate) {

    private fun tagRow(row: Row): TagDTO{
        return TagDTO(
            row.get("tag_id", String::class.java),
            row.get("name", String::class.java),
            row.get("color", String::class.java),
            row.get("confirmed", Boolean::class.javaObjectType),
            row.get("creator_id", String::class.java),
            row.get("updater_id", String::class.java),
            row.get("deleter_id", String::class.java)
        )
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    fun getAllTags(): Flow<TagDTO> {
        val query = """
            SELECT
                tag.id AS tag_id, tag.name AS name, tag.color AS color, tag.confirmed AS confirmed, tag.creator_id AS creator_id,
                tag.updater_id AS updater_id, tag.deleter_id AS deleter_id
            FROM tag
        """.trimIndent()

        return template.databaseClient
            .sql(query)
            .map { row, _ -> tagRow(row) }
            .all().asFlow()
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    suspend fun createTag(tagDTO: TagDTO, userId: String): TagDTO {
        val createdTag = template.insert(Tag(
            name = tagDTO.name,
            color = tagDTO.color,
            confirmed = true,
            creatorId = userId
        )).awaitSingle()
        return createdTag.toDTO()
    }

    suspend fun createNoConfirmedTag(tagDTO: TagDTO, userId: String): TagDTO {
        val createdTag = template.insert(Tag(
            name = tagDTO.name,
            color = tagDTO.color,
            confirmed = false,
            creatorId = userId
        )).awaitSingle()
        return createdTag.toDTO()
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    suspend fun confirmTag(userId: String, tagId: String) {
        template.update(
            Query.query(Criteria.where("id").`is`(tagId)),
            Update.update("updater_id", userId).set("confirmed", true),
            Tag::class.java).awaitSingle()
    }

    suspend fun updateTag(tagDTO: TagDTO, userId: String, tagId: String) {
        template.update(
            Query.query(Criteria.where("id").`is`(tagId)),
            Update.update("updater_id", userId).set("name", tagDTO.name)
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
        template.delete(
            Query.query(Criteria.where("id").`is`(tagId)),
            Tag::class.java).awaitSingle()
    }
}