package com.tyiu.corn.model.requests;

import com.tyiu.corn.model.enums.UserSkillLevel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserSkillRequest {
    private Long skillId;
    private UserSkillLevel level;
}
