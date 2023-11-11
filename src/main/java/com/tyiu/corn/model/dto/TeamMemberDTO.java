package com.tyiu.corn.model.dto;

import lombok.*;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamMemberDTO {
    private Long userId;
    private String email;
    private String firstName;
    private String lastName;
    private List<SkillDTO> skills;
}
