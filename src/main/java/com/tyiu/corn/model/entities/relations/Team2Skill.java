package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "team_skill")
public class Team2Skill {
    @Id
    private Long id;

    private Long teamId;
    private Long skillId;

    public Team2Skill(Long team, Long skill) {
        this.teamId = team;
        this.skillId = skill;
    }
}
