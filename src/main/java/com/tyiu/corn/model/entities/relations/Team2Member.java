package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "team_member")
public class Team2Member {
    @Id
    private Long id;

    private Long teamId;
    private Long memberId;

    public Team2Member(Long team, Long member) {
        this.teamId = team;
        this.memberId = member;
    }
}
