package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Profile {
    @Id
    private String id;
    private List<Scrum> scrums;
    private List<Idea> ideas;
    private List<Task> tasks;
    private Comment comment;

}
