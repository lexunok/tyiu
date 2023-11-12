package com.tyiu.corn.model.dto;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamMarketRequestDTO {
    private String id;
    private String ideaId;
    private String teamId;
    private Boolean accepted;

    private String name;
    private LocalDate updatedAt;
    private Boolean closed;
    private String description;
    private TeamMemberDTO owner;
    private TeamMemberDTO leader;
    private List<TeamMemberDTO> members;
    private List<SkillDTO> skills;
    private String letter;
}
