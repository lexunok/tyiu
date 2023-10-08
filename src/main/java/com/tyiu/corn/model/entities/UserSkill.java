package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.enums.UserSkillLevel;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Document
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserSkill {
    @Id
    private String id;
    @Indexed
    private String skillId;
    @Indexed
    private String userEmail;
    private SkillType type;
    private String name;
    private UserSkillLevel level;
}
