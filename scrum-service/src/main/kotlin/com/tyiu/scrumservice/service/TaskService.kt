package com.tyiu.scrumservice.service

import com.tyiu.scrumservice.model.Task
import com.tyiu.scrumservice.model.TaskRepository
import kotlinx.coroutines.flow.Flow
import org.springframework.stereotype.Service

@Service
class TaskService(private val repository: TaskRepository){

    fun getAllProjects(): Flow<Task> = repository.findAll()

}