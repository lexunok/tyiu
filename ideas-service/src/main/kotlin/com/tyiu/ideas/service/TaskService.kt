package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.TeamMemberDTO
import com.tyiu.ideas.model.dto.UserDTO
import io.r2dbc.spi.Row
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.toList
import kotlinx.coroutines.reactive.asFlow
import kotlinx.coroutines.reactive.awaitFirst
import kotlinx.coroutines.reactive.awaitFirstOrNull
import kotlinx.coroutines.reactor.awaitSingle
import kotlinx.coroutines.reactor.awaitSingleOrNull
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.r2dbc.core.delete
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import org.springframework.data.relational.core.query.Update.update
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import reactor.core.publisher.Flux
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.concurrent.ConcurrentHashMap

@Service
class TaskService
    (
    private val userRepository: UserRepository,
    private val tagRepository: TagRepository,
    val template: R2dbcEntityTemplate
    )
{
    suspend fun taskToDTO(task: Task): TaskDTO
    {
        val tasks = task.toDTO()

        tasks.initiator = (task.initiatorId?.let{userRepository.findById(it)})?.toDTO()
        tasks.executor = (task.executorId?.let{userRepository.findById(it)})?.toDTO()
        tasks.tags = task.id?.let { tagRepository.findAllTagByTaskId(it).toList().map { tag -> tag.toDTO() } }
        return tasks
    }

    private fun taskRow(row: Row, map: ConcurrentHashMap<String, TaskDTO>): TaskDTO?{
        return row.get("t_id", String::class.java)?.let {
            val task = map.getOrDefault(it,TaskDTO(
                it,
                row.get("t_sprint_id", String::class.java),
                row.get("t_project_id", String::class.java),
                row.get("t_position", Int::class.javaObjectType),
                row.get("t_name", String::class.java),
                row.get("t_description", String::class.java),
                row.get("t_leader_comment", String::class.java),
                UserDTO(
                    row.get("i_id", String::class.java),
                    row.get("i_email", String::class.java),
                    row.get("i_first_name", String::class.java),
                    row.get("i_last_name", String::class.java)
                ),
                UserDTO(
                    row.get("e_id", String::class.java),
                    row.get("e_email", String::class.java),
                    row.get("e_first_name", String::class.java),
                    row.get("e_last_name", String::class.java)
                ),
                row.get("t_work_hour", Int::class.javaObjectType),
                row.get("t_start_date", LocalDate::class.java),
                row.get("t_finish_date", LocalDate::class.java),
                listOf(),
                TaskStatus.valueOf(row.get("t_status", String::class.java)!!)
            ))
            row.get("tag_id", String::class.java)?.let { tagId ->
                val tagDTO = TagDTO(
                    tagId,
                    row.get("tag_name", String::class.java),
                    row.get("tag_color", String::class.java),
                    row.get("tag_confirmed", Boolean::class.javaObjectType),
                    row.get("tag_creator_id", String::class.java),
                    row.get("tag_updater_id", String::class.java),
                    row.get("tag_deleter_id", String::class.java)
                )
                task.tags = task.tags?.plus(tagDTO)
            }
            map[it] = task
            task
        }
    }

    private fun taskList(projectId: String, query: String): Flow<TaskDTO>{
        val map = ConcurrentHashMap<String, TaskDTO>()

        return template.databaseClient
            .sql(query)
            .bind("projectId",projectId)
            .map { row, _ -> taskRow(row, map) }
            .all()
            .thenMany(Flux.fromIterable(map.values)).asFlow()
    }

    //get
    fun getAllTasksByProject(projectId: String): Flow<TaskDTO> {
        val query = """
            SELECT
                t.id AS t_id, t.sprint_id AS t_sprint_id, t.project_id AS t_project_id, t.position AS t_position,
                t.name AS t_name, t.description AS t_description, t.leader_comment AS t_leader_comment,
                t.initiator_id AS t_initiator_id, t.executor_id AS t_executor_id, t.work_hour AS t_work_hour,
                t.start_date AS t_start_date, t.finish_date AS t_finish_date, t.status AS t_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                e.id AS e_id, e.email AS e_email, e.first_name AS e_first_name, e.last_name AS e_last_name,
                tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color, tag.confirmed AS tag_confirmed,
                tag.creator_id AS tag_creator_id, tag.updater_id AS tag_updater_id, tag.deleter_id AS tag_deleter_id
            FROM task t
                LEFT JOIN users i ON i.id = t.initiator_id
                LEFT JOIN users e ON e.id = t.executor_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id
                LEFT JOIN tag ON tag.id = tg.tag_id
            WHERE t.project_id = :projectId
            ORDER BY start_date ASC
        """.trimIndent()

        return taskList(projectId, query)
    }

    fun getAllTasksInBacklog(projectId: String): Flow<TaskDTO> {
        val query = """
            SELECT
                t.id AS t_id, t.sprint_id AS t_sprint_id, t.project_id AS t_project_id, t.position AS t_position,
                t.name AS t_name, t.description AS t_description, t.leader_comment AS t_leader_comment,
                t.initiator_id AS t_initiator_id, t.executor_id AS t_executor_id, t.work_hour AS t_work_hour,
                t.start_date AS t_start_date, t.finish_date AS t_finish_date, t.status AS t_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                e.id AS e_id, e.email AS e_email, e.first_name AS e_first_name, e.last_name AS e_last_name,
                tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color, tag.confirmed AS tag_confirmed,
                tag.creator_id AS tag_creator_id, tag.updater_id AS tag_updater_id, tag.deleter_id AS tag_deleter_id
            FROM task t
                LEFT JOIN users i ON i.id = t.initiator_id
                LEFT JOIN users e ON e.id = t.executor_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id
                LEFT JOIN tag ON tag.id = tg.tag_id
            WHERE t.project_id = :projectId AND status = 'InBackLog'
        """.trimIndent()

        return taskList(projectId, query)
    }

    fun getAllTasksInSprint(sprintId: String): Flow<TaskDTO> {
        val query = """
            SELECT
                t.id AS t_id, t.sprint_id AS t_sprint_id, t.project_id AS t_project_id, t.position AS t_position,
                t.name AS t_name, t.description AS t_description, t.leader_comment AS t_leader_comment,
                t.initiator_id AS t_initiator_id, t.executor_id AS t_executor_id, t.work_hour AS t_work_hour,
                t.start_date AS t_start_date, t.finish_date AS t_finish_date, t.status AS t_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                e.id AS e_id, e.email AS e_email, e.first_name AS e_first_name, e.last_name AS e_last_name,
                tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color, tag.confirmed AS tag_confirmed,
                tag.creator_id AS tag_creator_id, tag.updater_id AS tag_updater_id, tag.deleter_id AS tag_deleter_id
            FROM task t
                LEFT JOIN users i ON i.id = t.initiator_id
                LEFT JOIN users e ON e.id = t.executor_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id
                LEFT JOIN tag ON tag.id = tg.tag_id
            WHERE t.sprint_id = :sprintId
        """.trimIndent()

        val map = ConcurrentHashMap<String, TaskDTO>()

        return template.databaseClient
            .sql(query)
            .bind("sprintId",sprintId)
            .map { row, _ -> taskRow(row, map) }
            .all()
            .thenMany(Flux.fromIterable(map.values)).asFlow()
    }

    suspend fun getOneTaskById(taskId: String): TaskDTO? {
        val query = """
            SELECT
                t.id AS t_id, t.sprint_id AS t_sprint_id, t.project_id AS t_project_id, t.position AS t_position,
                t.name AS t_name, t.description AS t_description, t.leader_comment AS t_leader_comment,
                t.initiator_id AS t_initiator_id, t.executor_id AS t_executor_id, t.work_hour AS t_work_hour,
                t.start_date AS t_start_date, t.finish_date AS t_finish_date, t.status AS t_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                e.id AS e_id, e.email AS e_email, e.first_name AS e_first_name, e.last_name AS e_last_name,
                tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color, tag.confirmed AS tag_confirmed,
                tag.creator_id AS tag_creator_id, tag.updater_id AS tag_updater_id, tag.deleter_id AS tag_deleter_id
            FROM task t
                LEFT JOIN users i ON i.id = t.initiator_id
                LEFT JOIN users e ON e.id = t.executor_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id
                LEFT JOIN tag ON tag.id = tg.tag_id
            WHERE t.id = :taskId
        """.trimIndent()

        val map = ConcurrentHashMap<String, TaskDTO>()

        return template.databaseClient
            .sql(query)
            .bind("taskId",taskId)
            .map { row, _ -> taskRow(row, map) }
            .first()
            .thenMany(Flux.fromIterable(map.values))
            .awaitFirst()
    }

    //post
    suspend fun createTask(taskDTO: TaskDTO, userId: String): TaskDTO {
        val createdTask = template.insert(
            Task (
                sprintId = taskDTO.sprintId,
                projectId = taskDTO.projectId,
                position = if (taskDTO.sprintId == null) taskDTO.projectId?.let { template.count(query(where("project_id").`is`(it)
                    .and("status").`is`(TaskStatus.InBackLog.name)),
                    Task::class.java).awaitSingle().toInt() }?.plus(1) else null,
                name = taskDTO.name,
                description = taskDTO.description,
                leaderComment = taskDTO.leaderComment,
                initiatorId = userId,
                workHour = taskDTO.workHour,
                status = if (taskDTO.sprintId == null) TaskStatus.InBackLog else TaskStatus.NewTask
            )
        ).awaitSingle()

        taskDTO.id = createdTask.id
        taskDTO.status = createdTask.status
        taskDTO.position = createdTask.position
        taskDTO.initiator = UserDTO(createdTask.initiatorId, null, null, null)

        template.insert(
            TaskMovementLog(
                taskId = createdTask.id,
                executorId = taskDTO.executor?.id,
                userId = taskDTO.initiator?.id,
                startDate = LocalDateTime.now(),
                status = createdTask.status
            )
        ).awaitSingle()

        taskDTO.tags?.forEach {
            template.insert(
                Task2Tag(
                    taskId = createdTask.id,
                    tagId = it.id
                )
            ).awaitSingle()
        }
        return taskDTO
    }

    //put
    suspend fun putUpdateTask(taskId: String, taskDTO: TaskDTO) {
        template.update(query(where("id").`is`(taskId)),
            update("name", taskDTO.name)
                .set("description", taskDTO.description)
                .set("work_hour", taskDTO.workHour),
            Task::class.java).awaitSingle()
    }

    suspend fun putUpdateExecutorTask(taskId: String, executorId: String){
        template.update(query(where("id").`is`(taskId)),
            update("executor_id", executorId),
            Task::class.java).awaitSingle()
    }

    suspend fun updateLeaderCommentInTask(taskId: String, leaderComment: String){
        template.update(query(where("id").`is`(taskId)),
            update("leader_comment", leaderComment),
            Task::class.java).awaitSingle()
    }

    suspend fun updateDescriptionInTask(taskId: String, description: String){
        template.update(query(where("id").`is`(taskId)),
            update("description", description),
            Task::class.java).awaitSingle()
    }

    suspend fun updateNameInTask(taskId: String, name: String){
        template.update(query(where("id").`is`(taskId)),
            update("name", name),
            Task::class.java).awaitSingle()
    }

    suspend fun changePosition(taskId: String, position: Int){
        template.selectOne(query(where("id").`is`(taskId)), Task::class.java)
            .awaitSingle()
            .let {
                template.update(query(where("id").`is`(taskId)),
                    update("position", position),
                    Task::class.java).awaitSingle()
                if (it?.position!! > position){
                    template.databaseClient
                        .sql("UPDATE task SET position = position + 1 WHERE project_id = :projectId AND id <> :taskId AND position < :position AND position >= :newPosition AND status = 'InBackLog'")
                        .bind("projectId", it.projectId!!)
                        .bind("taskId", taskId)
                        .bind("position", it.position!!)
                        .bind("newPosition", position)
                        .await()
                }
                else if (it.position!! < position){
                    template.databaseClient
                        .sql("UPDATE task SET position = position - 1 WHERE project_id = :projectId AND id <> :taskId AND position > :position AND position <= :newPosition AND status = 'InBackLog'")
                        .bind("projectId", it.projectId!!)
                        .bind("taskId", taskId)
                        .bind("position", it.position!!)
                        .bind("newPosition", position)
                        .await()
                }
            }
    }

    //delete
    suspend fun deleteTask(taskId: String) {
        template.selectOne(query(where("id").`is`(taskId)), Task::class.java)
            .awaitSingle()
            .let {
                template.delete(query(where("id").`is`(taskId)), Task::class.java).awaitSingle()
                if (it.status == TaskStatus.InBackLog){
                    template.databaseClient
                        .sql("UPDATE task SET position = position - 1 WHERE project_id = :projectId AND id <> :taskId AND position > :position AND status = 'InBackLog'")
                        .bind("projectId", it.projectId!!)
                        .bind("taskId", taskId)
                        .bind("position", it.position!!)
                        .await()
                }
            }
    }

}
