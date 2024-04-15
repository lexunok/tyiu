package com.tyiu.ideas.model.dto;

import lombok.*;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamMemberDTO {
    private String userId;
    private String email;
    private String firstName;
    private String lastName;
    private List<SkillDTO> skills;
    public String getUserId() { return userId; }

}
