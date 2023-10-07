package com.tyiu.corn.model.responses;

import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.enums.UserSkillLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserSkillResponse {
    private String skillId;
    private String userEmail;
    private String name;
    private SkillType type;
    private UserSkillLevel level;
}
