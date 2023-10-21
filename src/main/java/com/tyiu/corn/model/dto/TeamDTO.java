package com.tyiu.corn.model.dto;

import lombok.*;

import java.time.Instant;
import java.time.LocalDate;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamDTO {
    private Long id;

    private String name;
    private String description;

    private Boolean closed;
    private Integer membersCount;
    private LocalDate createdAt;

    private TeamMemberDTO owner;
    private TeamMemberDTO leader;

    private List<TeamMemberDTO> members;
    private List<SkillDTO> skills;
}
