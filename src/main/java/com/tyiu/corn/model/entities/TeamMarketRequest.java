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
    private Long id;
    private Long ideaId;
    private Long teamId;
    private Boolean accepted;

    private String name;
    private LocalDate updatedAt;
    private Boolean closed;
    private String description;
    private Long ownerId;
    private Long leaderId;
    private String letter;
}
