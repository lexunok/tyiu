package com.tyiu.ideas.model

import kotlinx.coroutines.flow.Flow
import org.springframework.data.annotation.Id
import org.springframework.data.r2dbc.repository.Query
import org.springframework.data.relational.core.mapping.Table
import org.springframework.data.relational.core.sql.InsertBuilder.InsertValues
import org.springframework.data.repository.kotlin.CoroutineCrudRepository

//автоматическое создание task_to_tag вместе с task
interface TaskTagRepository: CoroutineCrudRepository<TaskTag, String> {
    @Query("DELETE FROM task_tag WHERE id=:tagId")
    fun deleteTagById(tagId: String): Flow<TaskTag>

    @Query("SELECT *  FROM task_tag WHERE project_id=:projectId")
    fun findAllTagByProjectId(projectId: String): Flow<TaskTag>

    @Query("SELECT * FROM task_tag JOIN task_to_tag ON task_tag.id = task_to_tag.tag_id WHERE task_to_tag.task_id = :taskId")
    fun findAllByTaskId(taskId: String): Flow<TaskTagDTO>

    @Query("INSERT INTO task_to_tag (task_id,tag_id) VALUES(taskId, tagId)")
    fun InsertValues(taskId: String?, tagId: String?)
} 

@Table
data class TaskTag (
    @Id
    val id: String? = null,
    val projectId: String? = null,
    var name: String? = null,
    var color: String? = null,
)

@Table("task_to_tag")
data class Task2Tag (
    val taskId: String? = null,
    val tagId: String? = null
)

data class TaskTagDTO (
    val id: String? = null,
    val projectId: String? = null,
    var name: String? = null,
    var color: String? = null,
)

fun TaskTag.toDTO(): TaskTagDTO = TaskTagDTO (
    id = id,
    projectId = projectId,
    name = name,
    color = color
)

data class TaskTagRequest(
    var tagName: String? = null,
    var tagColor: String? = null
)




