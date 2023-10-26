package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@AllArgsConstructor
@Table(name = "user_idea")
public class User2Idea {
    private Long userId;
    private Long ideaId;
}
