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
public class TeamInvitation {
    @Id
    private Long id;
    private String teamName;
    private Long teamId;
    private Long receiverId;
    private LocalDate createdAt;
}
