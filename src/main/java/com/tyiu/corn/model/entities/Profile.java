package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Setter
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document
public class Profile {
    @Id
    private String id;
    private String userEmail;
    private User user;
    private List<Skill> Skills;
//    private List<Project> Projects;
}
