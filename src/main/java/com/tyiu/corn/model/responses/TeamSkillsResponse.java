package com.tyiu.corn.model.responses;

import com.tyiu.corn.model.dto.SkillDTO;
import lombok.Data;

import java.util.List;

@Data
public class TeamSkillsResponse {
    private List<SkillDTO> skills;
    private List<SkillDTO> wantedSkills;
}
