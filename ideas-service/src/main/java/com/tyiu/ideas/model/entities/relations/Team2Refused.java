package com.tyiu.ideas.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "team_refused")
public class Team2Refused {
    private String teamId;
    private String userId;
}
