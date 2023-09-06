package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Comment;
import com.tyiu.corn.model.entities.Group;
import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.Date;
import java.util.List;
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class IdeaDTO {

    private String id;
    private String initiator;
    private String name;
    private Group experts;
    private Group projectOffice;
    private List<String> confirmedBy;
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
    private Long marketValue;
    private Long originality;
    private Long technicalRealizability;
    private Long preAssessment;
    private double rating;
    private double risk;
}
