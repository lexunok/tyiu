package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@AllArgsConstructor
@Table("idea_skill")
public class Idea2Skill {
    private String ideaId;
    private String skillId;
}
