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
    private String id;
    private String teamName;
    private String teamId;
    private String receiverId;
    private LocalDate createdAt;
}
