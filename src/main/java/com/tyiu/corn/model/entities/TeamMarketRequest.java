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
public class TeamMarketRequest {
    @Id
    private String id;
    private String ideaId;
    private String teamId;
    private Boolean accepted;

    private String name;
    private LocalDate updatedAt;
    private Boolean closed;
    private String description;
    private String ownerId;
    private String leaderId;
    private String letter;
}
