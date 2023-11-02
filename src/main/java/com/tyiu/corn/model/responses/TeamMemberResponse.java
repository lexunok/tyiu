package com.tyiu.corn.model.responses;

import com.tyiu.corn.model.dto.SkillDTO;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class TeamMemberResponse {
    private String email;
    private String firstName;
    private String lastName;

    private List<SkillDTO> skills;
}
