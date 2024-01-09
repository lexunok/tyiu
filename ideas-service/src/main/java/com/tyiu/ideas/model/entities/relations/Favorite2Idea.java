package com.tyiu.ideas.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Table("favorite_idea")
public class Favorite2Idea {
    private String userId;
    private String ideaMarketId;
}
