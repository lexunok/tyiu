package com.tyiu.corn.model.responses;

import com.tyiu.corn.model.enums.SkillType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProfileSkillResponse {
    private String id;
    private String name;
    private SkillType type;
}
