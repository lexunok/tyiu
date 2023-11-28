package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.Idea;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class IdeaDTO {
    private String id;
    private String initiatorEmail;
    private String name;
    private GroupDTO experts;
    private GroupDTO projectOffice;
    @Builder.Default
    private List<String> checkedBy = new ArrayList<>();
    private Idea.Status status;
    private LocalDateTime createdAt;
    private LocalDateTime modifiedAt;
    private Boolean isActive;
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
