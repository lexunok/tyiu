package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Profile;
import com.tyiu.corn.model.entities.Task;
import lombok.*;

import java.util.List;

@Data
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ScrumDTO {
        private Long id;
        private String name;
        private String description;
        private Integer count;
        private List<Profile> profiles;
        private List<Task> tasks;
    }
