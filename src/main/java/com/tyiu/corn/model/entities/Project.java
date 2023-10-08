package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Project {
    @Id
    private String id;

    private String name;
    private String description;

    @Indexed
    private String teamId;
    private Integer membersCount;

    private List<String> skills;
}
