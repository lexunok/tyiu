package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.dto.TeamMemberDTO;
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
public class ProjectRequest {
    @Id
    private String id;
    private String projectId;
    private TeamMemberDTO sender;
    private Instant createdAt;
}
