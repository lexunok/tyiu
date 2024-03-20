package com.tyiu.ideas.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

//автоматическое создание task_to_tag вместе с task
interface TagRepository: CoroutineCrudRepository<Tag, String> {
    @Query("DELETE FROM task_tag WHERE id=:tagId")
    fun deleteTagById(tagId: String): Flow<Tag>

    @Query("SELECT *  FROM task_tag WHERE project_id=:projectId")
    fun findAllTagByProjectId(projectId: String): Flow<Tag>

    @Query("SELECT * FROM task_tag JOIN task_to_tag ON task_tag.id = task_to_tag.tag_id WHERE task_to_tag.task_id = :taskId")
    fun findAllByTaskId(taskId: String): Flow<TagDTO>

    @Query("SELECT tag.* FROM task_tag JOIN tag ON tag.id = task_tag.tag_id WHERE task_tag = :taskId")
    fun findAllTagsInTaskByTaskId(taskId: String): Flow<Tag>
} 

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
    var confirmed: Boolean,
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

data class TaskTagRequest(
    var tagName: String? = null,
    var tagColor: String? = null
)




