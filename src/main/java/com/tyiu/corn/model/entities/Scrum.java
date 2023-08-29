package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Builder
@Table
public class Scrum {
    @Id
    private Long id;
    private String name;
    private String description;
    private Integer count;
    private List<Profile> profiles;
    private List<Task> tasks;
}
