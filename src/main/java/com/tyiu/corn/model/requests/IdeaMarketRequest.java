package com.tyiu.corn.model.requests;

import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
public class IdeaMarketRequest {
    private String id;
    private String initiatorEmail;
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
    private LocalDate startDate;
    private LocalDate finishDate;
}
