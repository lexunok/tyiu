package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.toList
import kotlinx.coroutines.reactor.awaitSingle
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.data.relational.core.query.Criteria.where
import org.springframework.data.relational.core.query.Query.query
import org.springframework.data.relational.core.query.Update.update
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import java.time.LocalDateTime


@Service
class TaskMovementLogService
    (
    private val taskMovementLogRepository: TaskMovementLogRepository,
    private val userRepository: UserRepository,
    private val taskRepository: TaskRepository,
    private val tagRepository: TagRepository,
    val template: R2dbcEntityTemplate,
    )
{
    private suspend fun toDTO(taskMovementLog: TaskMovementLog): TaskMovementLogDTO {
        val taskLog = taskMovementLog.toDTO()
        taskLog.task = taskMovementLog.taskId?.let {
            val taskDTO = taskRepository.findById(it)?.toDTO()
            taskDTO?.tags = tagRepository.findAllTagByTaskId(it).map { it1 -> it1.toDTO() }.toList()
            return@let taskDTO
        }
        taskLog.executor = taskMovementLog.executorId?.let { userRepository.findById(it)?.toDTO() }
        taskLog.user = taskMovementLog.userId?.let { userRepository.findById(it)?.toDTO() }
        return taskLog
    }

    fun getAllTaskLog(taskId: String): Flow<TaskMovementLogDTO> = taskMovementLogRepository.findAllByTaskId(taskId).map {toDTO(it)}

    suspend fun addNewTaskLog(taskMovementLogDTO: TaskMovementLogDTO): TaskMovementLogDTO {
        taskMovementLogDTO.let {
            it.task?.id.let { taskId ->
                if (taskId?.let { it1 -> taskMovementLogRepository.existsTaskMovementLogByTaskId(it1) } == true) {
                    template.databaseClient
                        .sql("UPDATE task_movement_log SET end_date = :finishDate WHERE task_id = :taskId")
                        .bind("finishDate", LocalDateTime.now())
                        .bind("taskId", taskId)
                        .await()
                }
                if (it.status == TaskStatus.NewTask){
                    template.update(query(where("id").`is`(taskId!!)),
                        update("status", it.status!!.toString())
                            .set("executor_id", null),
                        Task::class.java).awaitSingle()
                }
                else {
                    template.update(query(where("id").`is`(taskId!!)),
                        update("status", it.status!!.toString()),
                        Task::class.java).awaitSingle()
                }
            }
            val taskMovementLog = TaskMovementLog(
                taskId = it.task?.id,
                executorId = it.executor?.id,
                userId = it.user?.id,
                status = it.status
            )
            return toDTO(taskMovementLogRepository.save(taskMovementLog))
        }
    }
}
