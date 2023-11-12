package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.enums.StatusIdea;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class IdeaDTO {
    private String id;
    private String initiator;
    private String name;
    private GroupDTO experts;
    private GroupDTO projectOffice;
    private StatusIdea status;
    private LocalDateTime createdAt;
    private LocalDateTime modifiedAt;
    private String problem;
    private String solution;
    private String result;
    private String customer;
    private String contactPerson;
    private String description;
    private Long suitability;
    private Long budget;
    private Double preAssessment;
    private Double rating;
    private Short maxTeamSize;
    private Short minTeamSize;
}
