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
public class Team {
    @Id
    private String id;

    private String name;
    private String description;
    private Boolean closed;
    private Integer membersCount;

    private String ownerEmail;
    private String leaderEmail;

    private List<String> members;
    private List<String> skills;



}