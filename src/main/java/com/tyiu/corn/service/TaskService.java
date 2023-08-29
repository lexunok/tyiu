package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.TaskDTO;
import com.tyiu.corn.model.entities.Task;
import lombok.RequiredArgsConstructor;

import org.modelmapper.ModelMapper;
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
    private final ModelMapper mapper;
    //@Cacheable
    public Flux<TaskDTO> getListTask() {
        List<Task> tasks = taskRepository.findAll();
        return Flux.just(tasks).map(i -> mapper.map(i, TaskDTO.class));
    }

    //@CacheEvict(allEntries = true)
    public Mono<TaskDTO> saveTask(TaskDTO taskDTO) {
        taskDTO.setCreatedAt(new Date());
        Task task = taskRepository.save(mapper.map(taskDTO, Task.class));
        return Mono.just(mapper.map(task, TaskDTO.class));
    }

    //@CacheEvict(allEntries = true)
    public void deleteTask(Long taskId) {
        taskRepository.deleteById(taskId);
    }

    //@CacheEvict(allEntries = true)
    public void updateTask(Long taskId, TaskDTO updatedTask) {
        Task task = taskRepository.findById(taskId).orElseThrow();
        task.setTitle(updatedTask.getTitle());
        task.setDescription(updatedTask.getDescription());
        task.setAssignedTo(updatedTask.getAssignedTo());
        task.setPriority(updatedTask.getPriority());
        task.setDeadline(updatedTask.getDeadline());
        task.setStatus(updatedTask.getStatus());
        taskRepository.save(task);
    }


}
