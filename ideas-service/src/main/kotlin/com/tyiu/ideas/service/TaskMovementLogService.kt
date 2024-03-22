package com.tyiu.ideas.service

import com.tyiu.ideas.model.*
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service


@Service
class TaskMovementLogService
    (
    private val taskMovementLogRepository: TaskMovementLogRepository,
    private val userRepository: UserRepository,
    private val taskRepository: TaskRepository
    )
{
    private suspend fun toDTO(taskMovementLog: TaskMovementLog): TaskMovementLogDTO {
        val taskLog = taskMovementLog.toDTO()
        taskLog.task = taskMovementLog.taskId?.let { taskRepository.findById(it)?.toDTO() }
        taskLog.executor = taskMovementLog.executorId?.let { userRepository.findById(it)?.toDTO() }
        taskLog.user = taskMovementLog.userId?.let { userRepository.findById(it)?.toDTO() }
        return taskLog
    }

    fun getAllTaskLog(taskId: String): Flow<TaskMovementLogDTO> = taskMovementLogRepository.findAllByTaskId(taskId).map {toDTO(it)}

    suspend fun addNewTaskLog(taskMovementLogDTO: TaskMovementLogDTO): TaskMovementLogDTO {
        val taskMovementLog = TaskMovementLog(
            taskId = taskMovementLogDTO.task?.id,
            executorId = taskMovementLogDTO.executor?.id,
            userId = taskMovementLogDTO.user?.id,
            status = taskMovementLogDTO.status
        )

        return toDTO(taskMovementLogRepository.save(taskMovementLog))
    }
}
