package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@AllArgsConstructor
@Table(name = "team_desired_skill")
public class Team2DesiredSkill {
    private String teamId;
    private String skillId;
}
