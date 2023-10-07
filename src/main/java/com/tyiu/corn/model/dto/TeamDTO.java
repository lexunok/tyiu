package com.tyiu.corn.model.dto;

import lombok.*;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamDTO {
    private String id;
    private String name;
    private String description;
    private Boolean closed;
    private Integer membersCount;

    private TeamMemberDTO owner;
    private TeamMemberDTO leader;

    private List<TeamMemberDTO> members;
    private List<SkillDTO> skills;
}
