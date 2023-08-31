package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.TaskDTO;
import com.tyiu.corn.model.entities.Task;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import com.tyiu.corn.repository.TaskRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class TaskService {

    private final TaskRepository taskRepository;

    //@Cacheable
    public Flux<TaskDTO> getListTask() {
        return taskRepository.findAll().cast(TaskDTO.class);
    }

    //@CacheEvict(allEntries = true)
    public Mono<TaskDTO> saveTask(TaskDTO taskDTO) {
        taskDTO.setCreatedAt(new Date());
        return Mono.just(taskDTO).cast(Task.class).flatMap(taskRepository::save).cast(TaskDTO.class);
    }

    //@CacheEvict(allEntries = true)
    public void deleteTask(String taskId) {
        taskRepository.deleteById(taskId);
    }

    //@CacheEvict(allEntries = true)
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
