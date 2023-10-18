package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.Instant;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class ProjectInvitation {
    @Id
    private String id;

    private String projectName;
    private String projectId;
    private String receiverEmail;
    private Instant createdAt;
}
