package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;
import java.time.LocalDateTime;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Idea {
    @Id
    private Long id;
    private Long initiatorId;
    private String name;
    private Long groupExpertId;
    private Long groupProjectOfficeId;
    private Long companyId;
    private StatusIdea status;
    private LocalDateTime createdAt;
    private LocalDateTime modifiedAt;
    private ProjectType projectType;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String contactPerson;
    private String description;
    private Long suitability;
    private Long budget;
    private Long technicalRealizability;
    private Double preAssessment;
    private Double rating;
}