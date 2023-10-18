package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.SkillType;
import com.tyiu.corn.model.enums.UserSkillLevel;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.relational.core.mapping.Table;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class UserSkill {
    @Id
    private Long id;
    private Long skillId;
    private String userEmail;
    private SkillType type;
    private String name;
    private UserSkillLevel level;
}
