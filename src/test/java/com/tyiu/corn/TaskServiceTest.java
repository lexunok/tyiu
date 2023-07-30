package com.tyiu.corn;

import com.tyiu.corn.model.Task;
import com.tyiu.corn.repository.TaskRepository;
import com.tyiu.corn.service.TaskService;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.exceptions.base.MockitoException;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.internal.util.StringUtil.join;

public class TaskServiceTest{
    @InjectMocks
    private TaskService taskService;

    @Mock
    private TaskRepository taskRepository;

    @BeforeEach
    public void setUp() {
        try {
            MockitoAnnotations.openMocks(this).close();
        } catch (Exception e) {
            throw new MockitoException(
                    join(
                            "Failed to release mocks",
                            "",
                            "This should not happen unless you are using a third-party mock maker"),
                    e);
        }
        taskService = new TaskService(taskRepository);
    }

        @Test
        public void testListTask() {
            List<Task> expectedTasks = Arrays.asList(
                    //new Task(1L,"Task 1", "Description 1", "Assignee 1", 2, "2023-01-01", "In Progress"),
                    //new Task(2L, "Task 2", "Description 2", "Assignee 2", 3, "2023-01-02", "Pending")

            );

        Mockito.when(taskRepository.findAll()).thenReturn(expectedTasks);

        //List<Task> actualTasks = taskService.listTask();

        //assertEquals(expectedTasks.size(), actualTasks.size());
        //assertEquals(expectedTasks.get(0).getTitle(), actualTasks.get(0).getTitle());
    }

    @Test
    public void testSaveTask() {
        //Task task = new Task(1L, "Task", "Description", "Assignee", 1, "2023-01-01", "In Progress");

        //taskService.saveTask(task);

       // verify(taskRepository).save(task);
    }

    @Test
    public void testDeleteTask() {
        Long taskId = 1L;
        taskService.deleteTask(taskId);
        verify(taskRepository).deleteById(taskId);
    }

    @Test
    public void testUpdateTask() {
        Long taskId = 1L;
        //Task updatedTask = new Task(1L,"Updated Task", "Updated Description", "Updated Assignee", 1, "2022-02-01", "Complete");
        //Task task = new Task(2L,"Task", "Description", "Assignee", 3, "2022-01-01", "In Progress");

        //Mockito.when(taskRepository.findById(taskId)).thenReturn(Optional.of(task));

        //taskService.updateTask(taskId, updatedTask);

        //verify(taskRepository).save(task);
    }
}












