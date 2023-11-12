package com.tyiu.corn.model.entities;

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
    private String name;
    private String description;

    private Boolean closed;
    private LocalDate createdAt;

    private String ownerId;
    private String leaderId;
}