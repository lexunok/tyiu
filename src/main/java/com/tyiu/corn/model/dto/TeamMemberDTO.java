package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.entities.UserSkill;
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

    private List<UserSkill> skills;
}
