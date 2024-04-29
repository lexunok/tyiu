package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.UserDTO
import io.r2dbc.spi.Row
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.reactive.asFlow
import kotlinx.coroutines.reactive.awaitFirstOrNull
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.security.oauth2.jwt.Jwt
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import org.springframework.data.relational.core.query.Update.update
import org.springframework.stereotype.Service
import reactor.core.publisher.Flux.fromIterable
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.Comparator.comparingInt
import java.util.concurrent.ConcurrentHashMap

@Service
class SprintService (val template: R2dbcEntityTemplate)
{
    private fun sprintRow(row: Row, map: ConcurrentHashMap<String, SprintDTO>, map2: ConcurrentHashMap<String, TaskDTO>): SprintDTO?{
        return row.get("s_id", String::class.java)?.let { sprintId ->
            val sprint = map.getOrDefault(sprintId, SprintDTO(
                sprintId,
                row.get("s_project_id", String::class.java),
                row.get("s_name", String::class.java),
                row.get("s_report", String::class.java),
                SprintStatus.valueOf(row.get("s_status", String::class.java)!!),
                row.get("s_goal", String::class.java),
                row.get("s_start_date", LocalDate::class.java),
                row.get("s_finish_date", LocalDate::class.java),
                row.get("s_working_hours", Long::class.javaObjectType),
                listOf()
            ))
            row.get("t_id", String::class.java)?.let { taskId ->
                val task = map2.getOrDefault(taskId,TaskDTO(
                    taskId,
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
                    row.get("t_work_hour", Double::class.javaObjectType),
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
                    if (task.tags?.stream()?.noneMatch { t -> t.id.equals(tagDTO.id) }!!){
                        task.tags = task.tags?.plus(tagDTO)
                    }
                }
                map2[taskId] = task
                if (sprint.tasks?.stream()?.noneMatch { t -> t.id.equals(task.id) }!!){
                    sprint.tasks = sprint.tasks?.plus(task)
                }
            }
            map[sprintId] = sprint
            sprint
        }
    }


    fun getAllSprintsByProject(projectId: String): Flow<SprintDTO> {
        return template.select(query(where("project_id").`is`(projectId)), Sprint::class.java)
            .asFlow()
            .map { it.toDTO() }
    }

    suspend fun getSprintById(id: String): SprintDTO? {
        val query = """
            SELECT
                s.id AS s_id, s.project_id AS s_project_id, s.name AS s_name, s.goal AS s_goal,
                s.report AS s_report, s.start_date AS s_start_date, s.finish_date AS s_finish_date,
                s.working_hours AS s_working_hours, s.status AS s_status,
                t.id AS t_id, th.sprint_id AS t_sprint_id, t.project_id AS t_project_id, t.position AS t_position,
                t.name AS t_name, t.description AS t_description, t.leader_comment AS t_leader_comment,
                t.initiator_id AS t_initiator_id, th.executor_id AS t_executor_id, t.work_hour AS t_work_hour,
                t.start_date AS t_start_date, t.finish_date AS t_finish_date, th.status AS t_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                e.id AS e_id, e.email AS e_email, e.first_name AS e_first_name, e.last_name AS e_last_name,
                tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color, tag.confirmed AS tag_confirmed,
                tag.creator_id AS tag_creator_id, tag.updater_id AS tag_updater_id, tag.deleter_id AS tag_deleter_id
            FROM sprint s
                LEFT JOIN task_history th ON th.sprint_id = s.id
                LEFT JOIN task t ON t.id = th.task_id
                LEFT JOIN users i ON i.id = t.initiator_id
                LEFT JOIN users e ON e.id = th.executor_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id
                LEFT JOIN tag ON tag.id = tg.tag_id
            WHERE s.id = :sprintId
        """.trimIndent()

        val map = ConcurrentHashMap<String, SprintDTO>()
        val map2 = ConcurrentHashMap<String, TaskDTO>()

        return template.databaseClient
            .sql(query)
            .bind("sprintId", id)
            .map { row, _ -> sprintRow(row, map, map2) }
            .all()
            .thenMany(fromIterable(map.values))
            .awaitFirstOrNull()
    }

    suspend fun getActiveSprint(projectId: String): SprintDTO? {
        val query = """
            SELECT
                s.id AS s_id, s.project_id AS s_project_id, s.name AS s_name, s.goal AS s_goal,
                s.report AS s_report, s.start_date AS s_start_date, s.finish_date AS s_finish_date,
                s.working_hours AS s_working_hours, s.status AS s_status,
                t.id AS t_id, t.sprint_id AS t_sprint_id, t.project_id AS t_project_id, t.position AS t_position,
                t.name AS t_name, t.description AS t_description, t.leader_comment AS t_leader_comment,
                t.initiator_id AS t_initiator_id, t.executor_id AS t_executor_id, t.work_hour AS t_work_hour,
                t.start_date AS t_start_date, t.finish_date AS t_finish_date, t.status AS t_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                e.id AS e_id, e.email AS e_email, e.first_name AS e_first_name, e.last_name AS e_last_name,
                tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color, tag.confirmed AS tag_confirmed,
                tag.creator_id AS tag_creator_id, tag.updater_id AS tag_updater_id, tag.deleter_id AS tag_deleter_id
            FROM sprint s
                LEFT JOIN task t ON t.sprint_id = s.id
                LEFT JOIN users i ON i.id = t.initiator_id
                LEFT JOIN users e ON e.id = t.executor_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id
                LEFT JOIN tag ON tag.id = tg.tag_id
            WHERE s.project_id = :projectId AND s.status = 'ACTIVE'
        """.trimIndent()

        val map = ConcurrentHashMap<String, SprintDTO>()
        val map2 = ConcurrentHashMap<String, TaskDTO>()

        return template.databaseClient
            .sql(query)
            .bind("projectId", projectId)
            .map { row, _ -> sprintRow(row, map, map2) }
            .all()
            .thenMany(fromIterable(map.values))
            .awaitFirstOrNull()
    }

    fun getAllSprintMarks(sprintId: String): Flow<SprintMarkDTO> {
        val query = """
            SELECT
                sm.id AS sm_id, sm.project_id AS sm_project_id, sm.sprint_id AS sm_sprint_id,
                sm.user_id AS sm_user_id, sm.project_role AS sm_project_role, sm.mark AS sm_mark,
                u.first_name AS u_first_name, u.last_name AS u_last_name,
                t.id AS t_id, th.sprint_id AS t_sprint_id, t.project_id AS t_project_id, t.position AS t_position,
                t.name AS t_name, t.description AS t_description, t.leader_comment AS t_leader_comment,
                t.initiator_id AS t_initiator_id, th.executor_id AS t_executor_id, t.work_hour AS t_work_hour,
                t.start_date AS t_start_date, t.finish_date AS t_finish_date, th.status AS t_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                e.id AS e_id, e.email AS e_email, e.first_name AS e_first_name, e.last_name AS e_last_name,
                tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color, tag.confirmed AS tag_confirmed,
                tag.creator_id AS tag_creator_id, tag.updater_id AS tag_updater_id, tag.deleter_id AS tag_deleter_id
            FROM sprint_mark sm
                LEFT JOIN users u ON u.id = sm.user_id
                LEFT JOIN sprint_mark_task smt ON smt.sprint_mark_id = sm.id
                LEFT JOIN task_history th ON th.sprint_id = sm.sprint_id AND th.task_id = smt.task_id
                LEFT JOIN task t ON t.id = th.task_id
                LEFT JOIN users i ON i.id = t.initiator_id
                LEFT JOIN users e ON e.id = t.executor_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id
                LEFT JOIN tag ON tag.id = tg.tag_id
            WHERE sm.sprint_id = :sprintId
        """.trimIndent()

        val map = ConcurrentHashMap<String, SprintMarkDTO>()
        val map2 = ConcurrentHashMap<String, TaskDTO>()

        return template.databaseClient
            .sql(query)
            .bind("sprintId", sprintId)
            .map { row, _ ->
                return@map row.get("sm_id", String::class.java)?.let { sprintMarkId ->
                    val sprintMark = map.getOrDefault(sprintMarkId, SprintMarkDTO(
                        sprintMarkId,
                        row.get("sm_project_id", String::class.java),
                        row.get("sm_sprint_id", String::class.java),
                        row.get("sm_user_id", String::class.java),
                        row.get("u_first_name", String::class.java),
                        row.get("u_last_name", String::class.java),
                        ProjectRole.valueOf(row.get("sm_project_role", String::class.java)!!),
                        row.get("sm_mark", Double::class.javaObjectType),
                        listOf()
                    ))
                    row.get("t_id", String::class.java)?.let { taskId ->
                        val task = map2.getOrDefault(taskId, TaskDTO(
                            taskId,
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
                            row.get("t_work_hour", Double::class.javaObjectType),
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
                            if (task.tags?.stream()?.noneMatch { t -> t.id.equals(tagDTO.id) }!!){
                                task.tags = task.tags?.plus(tagDTO)
                            }
                        }
                        map2[taskId] = task
                        if (sprintMark.tasks?.stream()?.noneMatch { t -> t.id.equals(task.id) }!!){
                            sprintMark.tasks = sprintMark.tasks?.plus(task)
                        }
                    }
                    map[sprintMarkId] = sprintMark
                    sprintMark
                }
            }
            .all()
            .thenMany(fromIterable(map.values)).asFlow()
    }

    suspend fun createSprint(sprintDTO: SprintDTO, jwt: Jwt): SprintDTO {
        sprintDTO.projectId?.let {
            template.update(query(where("status").`is`(SprintStatus.ACTIVE.name)
                .and("project_id").`is`(it)),
                update("status", SprintStatus.DONE.name),
                Sprint::class.java)
        }
        val createdSprint = template.insert(
            Sprint(
                projectId = sprintDTO.projectId,
                name = sprintDTO.name,
                goal = sprintDTO.goal,
                workingHours = sprintDTO.workingHours,
                startDate = sprintDTO.startDate,
                finishDate = sprintDTO.finishDate
            )
        ).awaitSingle()
        sprintDTO.id = createdSprint.id
        sprintDTO.status = createdSprint.status
        sprintDTO.tasks?.forEach {
            template.update(query(where("id").`is`(it.id!!)),
                update("sprint_id", createdSprint.id!!)
                    .set("status", TaskStatus.NewTask.name),
                Task::class.java).awaitSingle()
            it.status = TaskStatus.NewTask
            it.sprintId = createdSprint.id
            template.update(query(where("task_id").`is`(it.id!!)
                .and("end_date").isNull),
                update("end_date", LocalDateTime.now()),
                TaskMovementLog::class.java).awaitSingle()
            template.insert(
                TaskMovementLog(
                    taskId = it.id,
                    executorId = it.executor?.id,
                    userId = jwt.id,
                    startDate = LocalDateTime.now(),
                    status = TaskStatus.NewTask
                )
            ).awaitSingle()
        }

        sprintDTO.projectId?.let {
            val tasks = template.select(query(where("project_id").`is`(it)
                .and("status").`is`(TaskStatus.InBackLog.name)), Task::class.java).sort(comparingInt { task -> task.position!! })
                .asFlow()
            var i = 1
            tasks.collect { task ->
                if (task.position != i){
                    task.id?.let { id ->
                        template.update(query(where("project_id").`is`(sprintDTO.projectId)
                            .and("status").`is`(TaskStatus.InBackLog.name)
                            .and("id").`is`(id)),
                            update("position", i),
                            Task::class.java) }
                }
                i += 1
            }
        }

        return sprintDTO
    }

    suspend fun addSprintMarks(sprintId: String, projectId: String, sprintMarks: Flow<SprintMarkDTO>) {
        sprintMarks.collect { sprintMark ->
            val createdSprintMark = template.insert(
                SprintMark(
                    projectId = projectId,
                    sprintId = sprintId,
                    userId = sprintMark.userId,
                    projectRole = sprintMark.projectRole,
                    mark = sprintMark.mark
                )
            ).awaitSingle()
            sprintMark.tasks?.forEach {
                template.insert(SprintMarkTask(createdSprintMark.id!!, it.id!!)).awaitSingle()
            }
            if (sprintMark.projectRole != ProjectRole.INITIATOR) {
                val marks = template.select(query(where("project_id").`is`(projectId)
                    .and("user_id").`is`(sprintMark.userId!!)), SprintMark::class.java).asFlow()
                val cnt = marks.toList().size
                if (template.exists(query(where("user_id").`is`(sprintMark.userId)
                        .and("project_id").`is`(projectId)), ProjectMarks::class.java).awaitSingle()){
                    template.update(query(where("project_id").`is`(projectId)
                        .and("user_id").`is`(sprintMark.userId)),
                        update("mark", (marks.toList().sumByDouble { it.mark!! } / cnt * 100.0).toLong() / 100.0),
                        ProjectMarks::class.java).awaitSingle()
                }
                else {
                    template.insert(
                        ProjectMarks(
                            projectId = projectId,
                            userId = sprintMark.userId,
                            mark = (marks.toList().sumByDouble { it.mark!! } / cnt * 100.0).toLong() / 100.0
                        )
                    ).awaitSingle()
                }
            }
        }
    }

    suspend fun updateSprintInfo(sprintId: String, sprintDTO: SprintDTO, userId: String){
        if (sprintDTO.goal != null){
            template.update(query(where("id").`is`(sprintId)),
                update("name", sprintDTO.name!!)
                    .set("goal", sprintDTO.goal)
                    .set("start_date", sprintDTO.startDate!!)
                    .set("finish_date", sprintDTO.finishDate!!)
                    .set("working_hours", sprintDTO.workingHours!!),
                Sprint::class.java).awaitSingle()
        }
        else {
            template.update(query(where("id").`is`(sprintId)),
                update("name", sprintDTO.name!!)
                    .set("start_date", sprintDTO.startDate!!)
                    .set("finish_date", sprintDTO.finishDate!!)
                    .set("working_hours", sprintDTO.workingHours!!),
                Sprint::class.java).awaitSingle()
        }
        template.update(query(where("sprint_id").`is`(sprintId)),
            update("sprint_id", null),
            Task::class.java).awaitSingle()
        sprintDTO.tasks?.forEach {
            template.update(query(where("id").`is`(it.id!!)),
                update("sprint_id", sprintId)
                    .set("position", null)
                    .set("status", if (it.status == TaskStatus.InBackLog) TaskStatus.NewTask.name else it.status!!.name),
                Task::class.java).awaitSingle()
            if (it.status == TaskStatus.InBackLog) {
                template.update(query(where("task_id").`is`(it.id!!)
                    .and("end_date").isNull),
                    update("end_date", LocalDateTime.now()),
                    TaskMovementLog::class.java).awaitSingle()
                template.insert(
                    TaskMovementLog(
                        taskId = it.id,
                        userId = userId,
                        startDate = LocalDateTime.now(),
                        status = TaskStatus.NewTask
                    )
                ).awaitSingle()
            }
            it.status = if (it.status == TaskStatus.InBackLog) TaskStatus.NewTask else it.status!!
        }
        val tasks = template.select(query(where("sprint_id").isNull
            .and("status").not(TaskStatus.InBackLog.name)), Task::class.java).asFlow()
        var pos = template.count(query(where("project_id").`is`(sprintDTO.projectId!!)
            .and("status").`is`(TaskStatus.InBackLog.name)), Task::class.java).awaitSingle().toInt()
        tasks.collect {
            pos = pos.plus(1)
            template.update(query(where("id").`is`(it.id!!)),
                update("position", pos)
                    .set("status", TaskStatus.InBackLog.name),
                Task::class.java).awaitSingle()
            template.update(query(where("task_id").`is`(it.id)
                .and("end_date").isNull),
                update("end_date", LocalDateTime.now()),
                TaskMovementLog::class.java).awaitSingle()
            template.insert(
                TaskMovementLog(
                    taskId = it.id,
                    userId = userId,
                    startDate = LocalDateTime.now(),
                    status = TaskStatus.InBackLog
                )
            ).awaitSingle()
        }
    }

    suspend fun putSprintFinish(sprintId: String, report: String, userId: String) {
        val tasks = template.select(query(where("sprint_id").`is`(sprintId)), Task::class.java).asFlow()
        var pos = tasks.first().projectId?.let { template.count(query(where("project_id").`is`(it)
            .and("status").`is`(TaskStatus.InBackLog.name)), Task::class.java).awaitSingle().toInt() }
        tasks.collect {
            template.insert(
                TaskHistory(
                    taskId = it.id,
                    sprintId = it.sprintId,
                    status = it.status,
                    executorId = it.executorId
                )
            ).awaitSingle()
            if (it.status!=TaskStatus.Done){
                pos = pos?.plus(1)
                template.update(query(where("id").`is`(it.id!!)),
                    update("sprint_id", null)
                        .set("position", pos)
                        .set("executor_id", null)
                        .set("status", TaskStatus.InBackLog.name),
                    Task::class.java).awaitSingle()
                template.insert(
                    TaskMovementLog(
                        taskId = it.id,
                        userId = userId,
                        startDate = LocalDateTime.now(),
                        status = TaskStatus.InBackLog
                    )
                ).awaitSingle()
            }
        }
        template.update(query(where("id").`is`(sprintId)),
            update("report", report)
                .set("status", SprintStatus.DONE.name)
                .set("finish_date", LocalDate.now()),
            Sprint::class.java).awaitSingle()
    }

    suspend fun deleteSprint(id: String, userId: String) {
        val tasks = template.select(query(where("sprint_id").`is`(id)), Task::class.java).asFlow()
        var pos = tasks.first().projectId?.let { template.count(query(where("project_id").`is`(it)
            .and("status").`is`(TaskStatus.InBackLog.name)), Task::class.java).awaitSingle().toInt() }
        tasks.collect {
            pos = pos?.plus(1)
            template.update(query(where("id").`is`(it.id!!)),
                update("sprint_id", null)
                    .set("position", pos)
                    .set("executor_id", null)
                    .set("status", TaskStatus.InBackLog.name),
                Task::class.java).awaitSingle()
            template.insert(
                TaskMovementLog(
                    taskId = it.id,
                    userId = userId,
                    startDate = LocalDateTime.now(),
                    status = TaskStatus.InBackLog
                )
            ).awaitSingle()
        }
        template.delete(query(where("id").`is`(id)), Sprint::class.java).awaitSingle()
    }
}