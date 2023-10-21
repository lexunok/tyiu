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
public class ProjectInvitation {
    @Id
    private Long id;

    private String projectName;
    private Long projectId;
    private Long receiverId;
    private LocalDate createdAt;
}
