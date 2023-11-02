package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.data.relational.core.mapping.Table;

@Data
@Table("user_skill")
@AllArgsConstructor
public class User2Skill {
    private Long userId;
    private Long skillId;
}
