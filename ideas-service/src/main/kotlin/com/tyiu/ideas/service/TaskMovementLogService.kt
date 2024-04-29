package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.UserDTO
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.reactive.asFlow
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import org.springframework.data.relational.core.query.Update.update
import org.springframework.stereotype.Service
import reactor.core.publisher.Flux
import java.time.Duration.between
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.concurrent.ConcurrentHashMap


@Service
class TaskMovementLogService(val template: R2dbcEntityTemplate)
{
    fun getAllTaskLog(taskId: String): Flow<TaskMovementLogDTO> {
        val query = """
            SELECT
                tml.id AS tml_id, tml.task_id AS tml_task_id, tml.executor_id AS tml_executor_id, tml.user_id AS tml_user_id,
                tml.start_date AS tml_start_date, tml.end_date AS tml_end_date, tml.status AS tml_status,
                t.id AS t_id, t.sprint_id AS t_sprint_id, t.project_id AS t_project_id, t.position AS t_position,
                t.name AS t_name, t.description AS t_description, t.leader_comment AS t_leader_comment,
                t.initiator_id AS t_initiator_id, t.executor_id AS t_executor_id, t.work_hour AS t_work_hour,
                t.start_date AS t_start_date, t.finish_date AS t_finish_date, t.status AS t_status,
                ti.id AS ti_id, ti.email AS ti_email, ti.first_name AS ti_first_name, ti.last_name AS ti_last_name,
                te.id AS te_id, te.email AS te_email, te.first_name AS te_first_name, te.last_name AS te_last_name,
                tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color, tag.confirmed AS tag_confirmed,
                tml_e.id AS tml_e_id, tml_e.email AS tml_e_email, tml_e.first_name AS tml_e_first_name, tml_e.last_name AS tml_e_last_name,
                tml_u.id AS tml_u_id, tml_u.email AS tml_u_email, tml_u.first_name AS tml_u_first_name, tml_u.last_name AS tml_u_last_name
            FROM task_movement_log tml
                LEFT JOIN task t ON t.id = tml.task_id
                LEFT JOIN users ti ON ti.id = t.initiator_id
                LEFT JOIN users te ON te.id = t.executor_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id
                LEFT JOIN tag ON tag.id = tg.tag_id
                LEFT JOIN users tml_e ON tml_e.id = tml.executor_id
                LEFT JOIN users tml_u ON tml_u.id = tml.user_id
            WHERE tml.task_id = :taskId
        """.trimIndent()

        val map = ConcurrentHashMap<String, TaskMovementLogDTO>()

        return template.databaseClient
            .sql(query)
            .bind("taskId", taskId)
            .map { row, _ ->
                row.get("tml_id", String::class.java)?.let {
                    val startDate = row.get("tml_start_date", LocalDateTime::class.java)
                    val endDate = row.get("tml_end_date", LocalDateTime::class.java)
                    val taskMovementLog = map.getOrDefault(it,TaskMovementLogDTO(
                        it,
                        TaskDTO(
                            row.get("t_id", String::class.java),
                            row.get("t_sprint_id", String::class.java),
                            row.get("t_project_id", String::class.java),
                            row.get("t_position", Int::class.javaObjectType),
                            row.get("t_name", String::class.java),
                            row.get("t_description", String::class.java),
                            row.get("t_leader_comment", String::class.java),
                            UserDTO(
                                row.get("ti_id", String::class.java),
                                row.get("ti_email", String::class.java),
                                row.get("ti_first_name", String::class.java),
                                row.get("ti_last_name", String::class.java)
                            ),
                            UserDTO(
                                row.get("te_id", String::class.java),
                                row.get("te_email", String::class.java),
                                row.get("te_first_name", String::class.java),
                                row.get("te_last_name", String::class.java)
                            ),
                            row.get("t_work_hour", Double::class.javaObjectType),
                            row.get("t_start_date", LocalDate::class.java),
                            row.get("t_finish_date", LocalDate::class.java),
                            listOf(),
                            TaskStatus.valueOf(row.get("t_status", String::class.java)!!)
                        ),
                        UserDTO(
                            row.get("tml_e_id", String::class.java),
                            row.get("tml_e_email", String::class.java),
                            row.get("tml_e_first_name", String::class.java),
                            row.get("tml_e_last_name", String::class.java)
                        ),
                        UserDTO(
                            row.get("tml_u_id", String::class.java),
                            row.get("tml_u_email", String::class.java),
                            row.get("tml_u_first_name", String::class.java),
                            row.get("tml_u_last_name", String::class.java)
                        ),
                        startDate,
                        endDate,
                        if (endDate != null) between(startDate, endDate).let { t -> "${t.toHours()}:${t.toMinutes() % 60}" }  else null,
                        TaskStatus.valueOf(row.get("tml_status", String::class.java)!!)
                    ))
                    row.get("tag_id", String::class.java)?.let { tagId ->
                        val tagDTO = TagDTO(
                            tagId,
                            row.get("tag_name", String::class.java),
                            row.get("tag_color", String::class.java),
                            row.get("tag_confirmed", Boolean::class.javaObjectType)
                        )
                        taskMovementLog.task?.tags = taskMovementLog.task?.tags?.plus(tagDTO)
                    }
                    map[it] = taskMovementLog
                    taskMovementLog
                }
            }
            .all()
            .thenMany(Flux.fromIterable(map.values))
            .sort(Comparator.comparing(TaskMovementLogDTO::startDate))
            .asFlow()
    }

    suspend fun addNewTaskLog(taskMovementLogDTO: TaskMovementLogDTO): TaskMovementLogDTO {
        taskMovementLogDTO.task!!.id!!.let { taskId ->
            template.exists(query(where("task_id").`is`(taskId)), TaskMovementLog::class.java)
                .awaitSingle()
                .let { isExists ->
                    if (isExists == true) {
                        template.update(query(where("task_id").`is`(taskId)),
                            update("end_date", LocalDateTime.now()),
                            TaskMovementLog::class.java).awaitSingle()
                    }
                }
            if (taskMovementLogDTO.status == TaskStatus.NewTask){
                template.update(query(where("id").`is`(taskId)),
                    update("status", taskMovementLogDTO.status!!.toString())
                        .set("executor_id", null),
                    Task::class.java).awaitSingle()
            }
            else {
                template.update(query(where("id").`is`(taskId)),
                    update("status", taskMovementLogDTO.status!!.toString()),
                    Task::class.java).awaitSingle()
            }
        }
        return template.insert(
            TaskMovementLog(
                taskId = taskMovementLogDTO.task?.id,
                executorId = if (taskMovementLogDTO.status == TaskStatus.NewTask) null else taskMovementLogDTO.executor?.id,
                userId = taskMovementLogDTO.user?.id,
                status = taskMovementLogDTO.status
            )
        ).awaitSingle()
            .let {
                taskMovementLogDTO.id = it.id
                taskMovementLogDTO.startDate = it.startDate
                taskMovementLogDTO.executor = if (it.status == TaskStatus.NewTask) null else taskMovementLogDTO.executor
                taskMovementLogDTO.task?.status = it.status
                return@let taskMovementLogDTO;
            }
    }
}
