package com.tyiu.ideas.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@AllArgsConstructor
@Table(name = "team_wanted_skill")
public class Team2WantedSkill {
    private String teamId;
    private String skillId;
}
