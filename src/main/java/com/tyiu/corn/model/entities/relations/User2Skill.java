package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@Table("user_skill")
@AllArgsConstructor
public class User2Skill {
    private Long userId;
    private Long skillId;
}
