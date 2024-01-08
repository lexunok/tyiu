package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@Getter
@NoArgsConstructor
@Table("user_skill")
@AllArgsConstructor
public class User2Skill {
    private String userId;
    private String skillId;
}
