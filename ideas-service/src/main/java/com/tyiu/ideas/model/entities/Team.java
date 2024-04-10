package com.tyiu.ideas.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Team {
    @Id
    private String id;
    private String marketId;
    private String name;
    private String description;
    private Boolean closed;
    private Boolean hasActiveProject;
    private LocalDate createdAt;
    private String ownerId;
    private String leaderId;

    public String getId() { return id; }
    public String getName() {
        return name;
    }
    public String getLeaderId() { return leaderId; }
}