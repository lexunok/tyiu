package com.tyiu.ideas.model.dto;


import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.responses.ProfileIdeaResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProfileDTO {
    String id;
    String email;
    String userTag;
    Boolean isUserTagVisible;
    String lastName;
    String firstName;
    LocalDateTime createdAt;
    List<Role> roles;
    List<SkillDTO> skills;
    List<ProfileIdeaResponse> ideas;
}
