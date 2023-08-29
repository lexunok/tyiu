package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Profile {
    @Id
    private Long id;
    private List<Scrum> scrums;
    private List<Idea> ideas;
    private List<Task> tasks;
    private Comment comment;

}
