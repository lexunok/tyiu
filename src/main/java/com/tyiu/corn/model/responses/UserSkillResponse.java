package com.tyiu.corn.model.responses;

import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.enums.UserSkillLevel;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UserSkillResponse {
    private String skillId;
    private String userEmail;
    private String name;
    private SkillType type;
    private UserSkillLevel level;
}
