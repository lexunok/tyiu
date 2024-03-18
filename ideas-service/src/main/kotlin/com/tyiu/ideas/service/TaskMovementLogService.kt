package com.tyiu.ideas.service

import com.tyiu.ideas.model.TaskMovementLog
import com.tyiu.ideas.model.TaskMovementLogDTO
import com.tyiu.ideas.model.TaskMovementLogRepository
import com.tyiu.ideas.model.toDTO
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map
import org.springframework.stereotype.Service


@Service
class TaskMovementLogService
    (
    private val repository: TaskMovementLogRepository,
    )
{
    suspend fun TaskMovementLogToDTO(tasksLog: TaskMovementLog): TaskMovementLogDTO {
        val tasksLog = tasksLog.toDTO()
        return tasksLog
    }

    fun getAllTaskLog(taskId: String): Flow<TaskMovementLogDTO> = repository.findAllByTaskId(taskId).map {TaskMovementLogToDTO(it)}
}
