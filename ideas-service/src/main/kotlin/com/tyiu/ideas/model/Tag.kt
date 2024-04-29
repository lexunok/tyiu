package com.tyiu.ideas.model

import org.springframework.data.annotation.Id
import org.springframework.data.relational.core.mapping.Table

@Table
data class Tag (
    @Id
    val id: String? = null,
    var name: String? = null,
    var color: String? = null,
    var confirmed: Boolean,
    var creatorId: String? = null,
    var updaterId: String? = null,
    var deleterId: String? = null
)

data class TagDTO (
    val id: String? = null,
    var name: String? = null,
    var color: String? = null,
    var confirmed: Boolean?,
    var creatorId: String? = null,
    var updaterId: String? = null,
    var deleterId: String? = null
)

fun Tag.toDTO(): TagDTO = TagDTO (
    id = id,
    name = name,
    color = color,
    confirmed = confirmed,
    creatorId = creatorId,
    updaterId = updaterId,
    deleterId = deleterId
)



