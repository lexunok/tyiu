package com.tyiu.corn.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProjectDTO {
    private String id;

    private String name;
    private String description;

    private TeamDTO team;
    private Integer membersCount;

    private List<SkillDTO> skills;
}
