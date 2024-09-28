package com.tyiu.ideas.service

import com.tyiu.client.exceptions.AccessException
import com.tyiu.client.models.Role
import com.tyiu.ideas.model.*
import com.tyiu.ideas.model.dto.IdeaMarketDTO
import com.tyiu.ideas.model.dto.TeamDTO
import com.tyiu.ideas.model.entities.Market
import com.tyiu.ideas.model.entities.Team
import com.tyiu.ideas.model.entities.relations.Team2Member
import io.r2dbc.spi.Row
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.reactive.asFlow
import kotlinx.coroutines.reactive.awaitFirst
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import org.springframework.data.relational.core.query.Update.update
import org.springframework.stereotype.Service
import reactor.core.publisher.Flux.fromIterable
import java.time.LocalDate
import java.util.concurrent.ConcurrentHashMap
import com.tyiu.client.models.UserDTO
import com.tyiu.ideas.model.entities.Idea
import com.tyiu.ideas.util.roleCheck

@Service
class ProjectService(val template: R2dbcEntityTemplate) {

    private fun projectRow(row: Row, map: ConcurrentHashMap<String, ProjectDTO>): ProjectDTO?{
        val memberId = row.get("pm_user_id", String::class.java)
        val markUserId = row.get("m_user_id", String::class.java)
        return row.get("p_id", String::class.java)?.let {
            val project = map.getOrDefault(it,ProjectDTO(
                it,
                row.get("p_idea_id", String::class.java),
                row.get("id_name", String::class.java),
                row.get("id_description", String::class.java),
                row.get("id_customer", String::class.java),
                UserDTO(
                    row.get("i_id", String::class.java),
                    row.get("i_email", String::class.java),
                    row.get("i_first_name", String::class.java),
                    row.get("i_last_name", String::class.java)
                ),
                TeamDTO(
                    row.get("t_id", String::class.java),
                    row.get("t_name", String::class.java),
                    row.get("t_members_count", Int::class.javaObjectType),
                ),
                listOf(),
                ReportProject(
                    it,
                    listOf(),
                    row.get("p_report", String::class.java),
                ),
                row.get("p_start_date", LocalDate::class.java),
                row.get("p_finish_date", LocalDate::class.java),
                ProjectStatus.valueOf(row.get("p_status", String::class.java)!!)
            ))
            if (memberId!=null) {
                val projectMemberDTO = ProjectMemberDTO(
                    memberId,
                    row.get("pm_team_id", String::class.java),
                    row.get("pms_email", String::class.java),
                    row.get("pms_first_name", String::class.java),
                    row.get("pms_last_name", String::class.java),
                    ProjectRole.valueOf(row.get("pm_project_role", String::class.java)!!),
                    row.get("pm_start_date", LocalDate::class.java),
                    row.get("pm_finish_date", LocalDate::class.java),
                )
                if (project.members?.stream()?.noneMatch{pm->pm.userId.equals(projectMemberDTO.userId)}!!) {
                    project.members = project.members?.plus(projectMemberDTO)
                }
            }
            if (markUserId!=null) {
                val projectMarksDTO = ProjectMarksDTO(
                    row.get("p_id", String::class.java),
                    markUserId,
                    row.get("mms_first_name", String::class.java),
                    row.get("mms_last_name", String::class.java),
                    ProjectRole.valueOf(row.get("pmms_project_role", String::class.java)!!),
                    row.get("m_mark", Double::class.javaObjectType),
                    listOf()
                )
                if (project.report?.marks?.stream()?.noneMatch { m -> m.userId.equals(projectMarksDTO.userId) }!!){
                    project.report?.marks = project.report?.marks?.plus(projectMarksDTO)
                }
                val taskId = row.get("tk_id", String::class.java)
                if (taskId!=null){
                    val taskDTO = TaskDTO(
                        id = row.get("tk_id", String::class.java),
                        projectId = it,
                        name = row.get("tk_name", String::class.java),
                        tags = listOf()
                    )
                    val tagId = row.get("tag_id", String::class.java)
                    projectMarksDTO.tasks = projectMarksDTO.tasks?.plus(taskDTO)
                    if (tagId!=null){
                        val tagDTO = TagDTO(
                            tagId,
                            row.get("tag_name", String::class.java),
                            row.get("tag_color", String::class.java),
                            null
                        )
                        taskDTO.tags = taskDTO.tags?.plus(tagDTO)
                    }
                }
            }
            map[it] = project
            project
        }
    }


    fun getAllProjects(): Flow<ProjectDTO> {
        val query = """
            SELECT
                p.id AS p_id, id.id AS p_idea_id, p.report AS p_report,p.start_date AS p_start_date, p.finish_date AS p_finish_date, p.status AS p_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                t.id AS t_id, t.name AS t_name, id.name AS id_name, id.description AS id_description,
                id.customer AS id_customer, pm.user_id AS pm_user_id, pm.team_id AS pm_team_id, pms.email AS pms_email,
                pms.first_name AS pms_first_name, pms.last_name AS pms_last_name, pm.project_role AS pm_project_role,
                pm.start_date AS pm_start_date, pm.finish_date AS pm_finish_date, m.user_id AS m_user_id,
                mms.first_name AS mms_first_name, mms.last_name AS mms_last_name, pmms.project_role AS pmms_project_role, m.mark AS m_mark,
                tk.id AS tk_id, tk.name AS tk_name, tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color,
                (SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) AS t_members_count
            FROM project p
                LEFT JOIN team t ON t.id = p.team_id
                LEFT JOIN idea id ON id.id = p.idea_id
                LEFT JOIN users i ON i.id = id.initiator_id
                LEFT JOIN project_member pm ON pm.project_id = p.id
                LEFT JOIN users pms ON pms.id = pm.user_id
                LEFT JOIN project_marks m ON m.project_id = p.id
                LEFT JOIN users mms ON mms.id = m.user_id
                LEFT JOIN project_member pmms ON pmms.user_id = mms.id
                LEFT JOIN task tk ON tk.project_id = p.id AND tk.executor_id = m.user_id
                LEFT JOIN task_tag tg ON tg.task_id = tk.id
                LEFT JOIN tag ON tag.id = tg.tag_id
            ORDER BY p.start_date ASC
        """.trimIndent()

        val map = ConcurrentHashMap<String, ProjectDTO>()

        return template.databaseClient
            .sql(query)
            .map { row, _ -> projectRow(row, map) }
            .all()
            .thenMany(fromIterable(map.values)).asFlow()
    }

    fun getYourProjects(userId: String): Flow<ProjectDTO> {
        val query = """
            SELECT
                p.id AS p_id, id.id AS p_idea_id, p.report AS p_report,p.start_date AS p_start_date, p.finish_date AS p_finish_date, p.status AS p_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                t.id AS t_id, t.name AS t_name, id.name AS id_name, id.description AS id_description,
                id.customer AS id_customer, pm.user_id AS pm_user_id, pm.team_id AS pm_team_id, pms.email AS pms_email,
                pms.first_name AS pms_first_name, pms.last_name AS pms_last_name, pm.project_role AS pm_project_role,
                pm.start_date AS pm_start_date, pm.finish_date AS pm_finish_date, m.user_id AS m_user_id,
                mms.first_name AS mms_first_name, mms.last_name AS mms_last_name, pmms.project_role AS pmms_project_role, m.mark AS m_mark,
                tk.id AS tk_id, tk.name AS tk_name, tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color,
                (SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) AS t_members_count
            FROM project p
                LEFT JOIN team t ON t.id = p.team_id
                LEFT JOIN idea id ON id.id = p.idea_id
                LEFT JOIN users i ON i.id = id.initiator_id
                LEFT JOIN project_member pm ON pm.project_id = p.id
                LEFT JOIN users pms ON pms.id = pm.user_id
                LEFT JOIN project_marks m ON m.project_id = p.id
                LEFT JOIN users mms ON mms.id = m.user_id
                LEFT JOIN project_member pmms ON pmms.user_id = mms.id
                LEFT JOIN task tk ON tk.project_id = p.id AND tk.executor_id = m.user_id
                LEFT JOIN task_tag tg ON tg.task_id = tk.id
                LEFT JOIN tag ON tag.id = tg.tag_id
                JOIN project_member ON p.id = project_member.project_id 
            WHERE project_member.user_id = :userId
            ORDER BY p.start_date ASC
        """.trimIndent()

        val map = ConcurrentHashMap<String, ProjectDTO>()

        return template.databaseClient
            .sql(query)
            .bind("userId",userId)
            .map { row, _ -> projectRow(row, map) }
            .all()
            .thenMany(fromIterable(map.values)).asFlow()
    }

    fun getYourActiveProjects(userId: String): Flow<ProjectDTO> {
        val query = """
            SELECT
                p.id AS p_id, id.id AS p_idea_id, p.report AS p_report,p.start_date AS p_start_date, p.finish_date AS p_finish_date, p.status AS p_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                t.id AS t_id, t.name AS t_name, id.name AS id_name, id.description AS id_description,
                id.customer AS id_customer, pm.user_id AS pm_user_id, pm.team_id AS pm_team_id, pms.email AS pms_email,
                pms.first_name AS pms_first_name, pms.last_name AS pms_last_name, pm.project_role AS pm_project_role,
                pm.start_date AS pm_start_date, pm.finish_date AS pm_finish_date, m.user_id AS m_user_id,
                mms.first_name AS mms_first_name, mms.last_name AS mms_last_name, pmms.project_role AS pmms_project_role, m.mark AS m_mark,
                tk.id AS tk_id, tk.name AS tk_name, tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color,
                (SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) AS t_members_count
            FROM project p
                LEFT JOIN team t ON t.id = p.team_id
                LEFT JOIN idea id ON id.id = p.idea_id
                LEFT JOIN users i ON i.id = id.initiator_id
                LEFT JOIN project_member pm ON pm.project_id = p.id
                LEFT JOIN users pms ON pms.id = pm.user_id
                LEFT JOIN project_marks m ON m.project_id = p.id
                LEFT JOIN users mms ON mms.id = m.user_id
                LEFT JOIN project_member pmms ON pmms.user_id = mms.id
                LEFT JOIN task tk ON tk.project_id = p.id AND tk.executor_id = m.user_id
                LEFT JOIN task_tag tg ON tg.task_id = tk.id
                LEFT JOIN tag ON tag.id = tg.tag_id
                JOIN project_member ON p.id = project_member.project_id 
            WHERE project_member.user_id = :userId AND p.status = 'ACTIVE'
            ORDER BY p.start_date ASC
        """.trimIndent()

        val map = ConcurrentHashMap<String, ProjectDTO>()

        return template.databaseClient
            .sql(query)
            .bind("userId",userId)
            .map { row, _ -> projectRow(row, map) }
            .all()
            .thenMany(fromIterable(map.values)).asFlow()
    }

    suspend fun getOneProject(projectId: String): ProjectDTO? {
        val query = """
            SELECT
                p.id AS p_id, id.id AS p_idea_id, p.report AS p_report,p.start_date AS p_start_date, p.finish_date AS p_finish_date, p.status AS p_status,
                i.id AS i_id, i.email AS i_email, i.first_name AS i_first_name, i.last_name AS i_last_name,
                t.id AS t_id, t.name AS t_name, id.name AS id_name, id.description AS id_description,
                id.customer AS id_customer, pm.user_id AS pm_user_id, pm.team_id AS pm_team_id, pms.email AS pms_email,
                pms.first_name AS pms_first_name, pms.last_name AS pms_last_name, pm.project_role AS pm_project_role,
                pm.start_date AS pm_start_date, pm.finish_date AS pm_finish_date, m.user_id AS m_user_id,
                mms.first_name AS mms_first_name, mms.last_name AS mms_last_name, pmms.project_role AS pmms_project_role, m.mark AS m_mark,
                tk.id AS tk_id, tk.name AS tk_name, tag.id AS tag_id, tag.name AS tag_name, tag.color AS tag_color,
                (SELECT COUNT(*) FROM team_member WHERE team_id = t.id AND finish_date IS NULL) AS t_members_count
            FROM project p
                LEFT JOIN team t ON t.id = p.team_id
                LEFT JOIN idea id ON id.id = p.idea_id
                LEFT JOIN users i ON i.id = id.initiator_id
                LEFT JOIN project_member pm ON pm.project_id = p.id
                LEFT JOIN users pms ON pms.id = pm.user_id
                LEFT JOIN project_marks m ON m.project_id = p.id
                LEFT JOIN users mms ON mms.id = m.user_id
                LEFT JOIN project_member pmms ON pmms.user_id = mms.id
                LEFT JOIN task tk ON tk.project_id = p.id AND tk.executor_id = m.user_id
                LEFT JOIN task_tag tg ON tg.task_id = tk.id
                LEFT JOIN tag ON tag.id = tg.tag_id 
            WHERE p.id = :projectId
            ORDER BY p.start_date ASC
        """.trimIndent()

        val map = ConcurrentHashMap<String, ProjectDTO>()

        return template.databaseClient
            .sql(query)
            .bind("projectId",projectId)
            .map { row, _ -> projectRow(row, map) }
            .all()
            .thenMany(fromIterable(map.values)).awaitFirst()
    }

    fun getProjectMembers(projectId: String): Flow<ProjectMemberDTO> {
        val query = """
            SELECT
                pm.project_id AS project_id, pm.user_id AS user_id, pm.team_id AS team_id, u.email AS email,
                u.first_name AS first_name, u.last_name AS last_name, pm.project_role AS project_role,
                pm.start_date AS start_date, pm.finish_date AS finish_date
            FROM project_member pm
                LEFT JOIN users u ON u.id = pm.user_id
            WHERE pm.project_id = :projectId
        """.trimIndent()

        return template.databaseClient
            .sql(query)
            .bind("projectId",projectId)
            .map { row, _ -> ProjectMemberDTO(
                        row.get("user_id", String::class.java),
                        row.get("team_id", String::class.java),
                        row.get("email", String::class.java),
                        row.get("first_name", String::class.java),
                        row.get("last_name", String::class.java),
                        ProjectRole.valueOf(row.get("project_role", String::class.java)!!),
                        row.get("start_date", LocalDate::class.java),
                        row.get("finish_date", LocalDate::class.java),
                    )}
            .all().asFlow()
    }

    fun getProjectMarks(projectId: String): Flow<ProjectMarksDTO> {
        val query = """
            SELECT
                pms.project_id AS project_id, pms.user_id AS user_id,u.first_name AS first_name, u.last_name AS last_name,
                pm.project_role AS project_role, pms.mark AS mark, t.id AS t_id, t.name AS t_name, tag.id AS tag_id, 
                tag.name AS tag_name, tag.color AS tag_color
            FROM project_marks pms
                LEFT JOIN users u ON u.id = pms.user_id
                LEFT JOIN project_member pm ON pm.project_id = pms.project_id AND pm.user_id = pms.user_id
                LEFT JOIN task t ON t.project_id = pms.project_id AND t.executor_id = pms.user_id
                LEFT JOIN task_tag tg ON tg.task_id = t.id 
                LEFT JOIN tag ON tag.id = tg.tag_id
            WHERE pms.project_id = :projectId
        """.trimIndent()

        val map = ConcurrentHashMap<String, ProjectMarksDTO>()

        return template.databaseClient
            .sql(query)
            .bind("projectId",projectId)
            .map { row, _ ->
                row.get("user_id", String::class.java)?.let {
                    val projectMarksDTO = map.getOrDefault(it,ProjectMarksDTO(
                        row.get("project_id", String::class.java),
                        it,
                        row.get("first_name", String::class.java),
                        row.get("last_name", String::class.java),
                        ProjectRole.valueOf(row.get("project_role", String::class.java)!!),
                        row.get("mark", Double::class.javaObjectType),
                        listOf()
                    ))
                    val taskId = row.get("t_id", String::class.java)
                    if (taskId!=null){
                        val taskDTO = TaskDTO(
                            id = taskId,
                            projectId = it,
                            name = row.get("t_name", String::class.java),
                            tags = listOf()
                        )
                        val tagId = row.get("tag_id", String::class.java)
                        projectMarksDTO.tasks = projectMarksDTO.tasks?.plus(taskDTO)
                        if (tagId!=null){
                            val tagDTO = TagDTO(
                                tagId,
                                row.get("tag_name", String::class.java),
                                row.get("tag_color", String::class.java),
                                null
                            )
                            taskDTO.tags = taskDTO.tags?.plus(tagDTO)
                        }
                    }
                    map[it] = projectMarksDTO
                    projectMarksDTO

            }}
            .all().thenMany(fromIterable(map.values)).asFlow()
    }


    suspend fun createProject(ideaMarketDTO: IdeaMarketDTO): ProjectDTO {
        val createdProject = template.insert(
            Project(
                ideaId = ideaMarketDTO.ideaId,
                teamId = ideaMarketDTO.team.id,
                finishDate = template.selectOne(query(where("id").`is`(ideaMarketDTO.marketId)),Market::class.java).awaitSingle().finishDate
            )
        ).awaitSingle()

        val leaderId = template.selectOne(query(where("id").`is`(ideaMarketDTO.team.id)), Team::class.java).awaitSingle().leaderId

        template.select(query(where("team_id").`is`(ideaMarketDTO.team.id)),Team2Member::class.java)
            .asFlow()
            .collect {
                template.insert(
                    ProjectMember(
                        projectId = createdProject.id,
                        userId = it.memberId,
                        teamId = ideaMarketDTO.team.id,
                        projectRole = if (it.memberId == leaderId) ProjectRole.TEAM_LEADER else ProjectRole.MEMBER,
                        finishDate = createdProject.finishDate
                    )
                ).awaitSingle()
            }

        template.insert(
            ProjectMember(
                projectId = createdProject.id,
                userId = ideaMarketDTO.initiator.id,
                projectRole = ProjectRole.INITIATOR,
                finishDate = createdProject.finishDate
            )
        ).awaitSingle()

        return createdProject.toDTO()
    }



    suspend fun addMembersInProject(projectId: String, addToProjectRequest: AddToProjectRequest ): ProjectMemberDTO {
            return template.insert(
                ProjectMember(
                    projectId = projectId,
                    userId = addToProjectRequest.userId,
                    teamId = addToProjectRequest.teamId,
                    finishDate = template.selectOne(query(where("id").`is`(projectId)),Project::class.java).awaitSingle().finishDate
                )
            ).awaitSingle().toDTO()
    }


    suspend fun pauseProject(projectId: String) {
        template.update(query(where("id").`is`(projectId)),
            update("status", ProjectStatus.PAUSED.name),
            Project::class.java).awaitSingle()
    }

    suspend fun putFinishProject(projectId: String, report: String, userId: String, roles: List<String>) {
        template.selectOne(query(where("id").`is`(projectId)), Project::class.java)
            .awaitSingle()
            .let { project ->
                template.selectOne(query(where("id").`is`(project.ideaId!!)), Idea::class.java)
                    .awaitSingle()
                    .let { idea ->
                        template.selectOne(query(where("id").`is`(project.teamId!!)), Team::class.java)
                            .awaitSingle()
                            .let { team ->
                                if (roles.roleCheck(listOf(Role.ADMIN, Role.PROJECT_OFFICE))
                                    || team.leaderId.equals(userId)
                                    || idea.initiatorId.equals(userId)) {
                                    template.update(query(where("id").`is`(project.teamId)),
                                        update("has_active_project", false),
                                        Team::class.java).awaitSingle()
                                    project.status = ProjectStatus.DONE
                                    project.report = report
                                    template.update(project).awaitSingle()
                                } else {
                                    println("вызвался")
                                    throw AccessException("Доступ запрещен")
                                }
                            }
                    }
            }
    }
}



