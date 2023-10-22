package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@AllArgsConstructor
@Table(name = "team_member")
public class Team2Member {

    private Long teamId;
    private Long memberId;

}
