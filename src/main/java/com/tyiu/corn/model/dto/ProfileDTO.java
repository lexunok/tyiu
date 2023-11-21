package com.tyiu.corn.model.dto;


import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.responses.ProfileIdeaResponse;
import com.tyiu.corn.model.responses.ProfileProjectResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProfileDTO {
    String id;
    String email;
    String lastName;
    String firstName;
    List<Role> roles;
    List<SkillDTO> skills;
    List<ProfileIdeaResponse> ideas;
    List<ProfileProjectResponse> projects;
}
