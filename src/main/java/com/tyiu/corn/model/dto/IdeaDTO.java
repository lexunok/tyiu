package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Comment;
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
    private ProjectType projectType;
    private String experts;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String contactPerson;
    private String description;
    private Long realizability;
    private Long suitability;
    private Long budget;
    private Long preAssessment;
    private StatusIdea status;
    private double rating;
    private double risk;
    private Instant createdAt;
    private Instant modifiedAt;
    private String marketValue;
    private String originality;
    private String technicalFeasibility;
    private String understanding;

}
