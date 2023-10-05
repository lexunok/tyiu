package com.tyiu.corn.model.requests;

import com.tyiu.corn.model.enums.UserSkillLevel;
import lombok.Data;

@Data
public class UserSkillRequest {
    private String skillId;
    private UserSkillLevel level;
}
