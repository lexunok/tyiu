package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Task;
import com.tyiu.corn.model.entities.User;

import lombok.*;

import java.util.List;

@Data
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ScrumDTO {
        private String id;
        private String name;
        private String description;
        private Integer count;
        private List<UserDTO> profiles;
        private List<Task> tasks;
    }
