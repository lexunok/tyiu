package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.TaskDTO;
import com.tyiu.corn.model.entities.Task;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import com.tyiu.corn.repository.TaskRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


import java.util.Date;

@Service
@RequiredArgsConstructor
public class TaskService {

    private final TaskRepository taskRepository;

    public Flux<TaskDTO> getListTask() {
        return taskRepository.findAll().cast(TaskDTO.class);
    }

    public Mono<TaskDTO> saveTask(TaskDTO taskDTO) {
        taskDTO.setCreatedAt(new Date());
        return Mono.just(taskDTO).cast(Task.class).flatMap(taskRepository::save).cast(TaskDTO.class);
    }

    public void deleteTask(String taskId) {
        taskRepository.deleteById(taskId);
    }

    public void updateTask(String taskId, TaskDTO updatedTask) {
        Mono<Task> task = taskRepository.findById(taskId);
        task.flatMap(t -> {
            t.setTitle(updatedTask.getTitle());
            t.setDescription(updatedTask.getDescription());
            t.setAssignedTo(updatedTask.getAssignedTo());
            t.setPriority(updatedTask.getPriority());
            t.setDeadline(updatedTask.getDeadline());
            t.setStatus(updatedTask.getStatus());
            return taskRepository.save(t);
        });
    }


}
