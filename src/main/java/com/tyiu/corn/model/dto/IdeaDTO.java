package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class IdeaDTO {
    private Long id;
    private String initiator;
    private String name;
    private Group experts;
    private Group projectOffice;
    private StatusIdea status;
    private Instant createdAt;
    private Instant modifiedAt;
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
