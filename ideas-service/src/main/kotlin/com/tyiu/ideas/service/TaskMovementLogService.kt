package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.toList
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate
import org.springframework.r2dbc.core.await
import org.springframework.stereotype.Service
import java.time.LocalDate


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
            it.task?.id.let {taskId ->
                if (taskId?.let { it1 -> taskMovementLogRepository.existsTaskMovementLogByTaskId(it1) } == true) {
                    template.databaseClient
                        .sql("UPDATE task_movement_log SET end_date = :finishDate WHERE task_id = :taskId")
                        .bind("finishDate", LocalDate.now())
                        .bind("taskId", taskId)
                        .await()
                }
                template.databaseClient
                    .sql("UPDATE task SET status = :status WHERE id = :taskId")
                    .bind("status", it.status!!.toString())
                    .bind("taskId", taskId!!)
                    .await()
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
