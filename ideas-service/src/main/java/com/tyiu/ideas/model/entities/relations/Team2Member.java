package com.tyiu.ideas.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "team_member")
public class Team2Member {
    private String teamId;
    private String memberId;
    private Boolean isActive;
    private LocalDate startDate;
    private LocalDate finishDate;

    public String getMemberId() { return memberId; }
}
