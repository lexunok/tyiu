package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Builder
@Document
public class Scrum {
    @Id
    private String id;
    private String name;
    private String description;
    private Integer count;
    private List<String> profilesId;
    private List<String> tasksId;
}
