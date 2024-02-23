package com.tyiu.ideas.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamExperienceDTO {
    private String teamId;
    private String teamName;
    private String userId;
    private String firstName;
    private String lastName;
    private LocalDate startDate;
    private LocalDate finishDate;
    private Boolean hasActiveProject;
}
