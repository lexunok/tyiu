package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Table("idea_checked")
public class Idea2Checked {
    private String userId;
    private String ideaId;
}
