package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Row;

import java.time.LocalDate;
import java.util.*;
import java.util.function.BiFunction;

public class TeamMapper implements BiFunction<Row, Object, TeamDTO> {

    private final Map<Long, TeamDTO> teamDTOMap = new LinkedHashMap<>();

    @Override
    public TeamDTO apply(Row row, Object o) {
        Long teamId = row.get("id", Long.class);
        TeamDTO existingTeam = teamDTOMap.get(teamId);

        if (existingTeam == null) {
            existingTeam = TeamDTO.builder()
                    .id(teamId)
                    .name(row.get("name", String.class))
                    .description(row.get("description", String.class))
                    .closed(row.get("closed", Boolean.class))
                    .createdAt(row.get("created_at", LocalDate.class))
                    .owner(TeamMemberDTO.builder()
                            .userId(row.get("o_id", Long.class))
                            .email(row.get("o_email", String.class))
                            .firstName(row.get("o_first_name", String.class))
                            .lastName(row.get("o_last_name", String.class))
                            .build())
                    .leader(TeamMemberDTO.builder()
                            .userId(row.get("l_id", Long.class))
                            .email(row.get("l_email", String.class))
                            .firstName(row.get("l_first_name", String.class))
                            .lastName(row.get("l_last_name", String.class))
                            .build())
                    .members(new ArrayList<>())
                    .skills(new ArrayList<>())
                    .build();
            teamDTOMap.put(teamId, existingTeam);
        }

        TeamMemberDTO member = TeamMemberDTO.builder()
                .userId(row.get("m_id", Long.class))
                .email(row.get("m_email", String.class))
                .firstName(row.get("m_first_name", String.class))
                .lastName(row.get("m_last_name", String.class))
                .build();

        if(existingTeam.getMembers().stream().noneMatch(m -> m.getUserId().equals(member.getUserId()))) {
            existingTeam.getMembers().add(member);
            existingTeam.setMembersCount(existingTeam.getMembers().size());
        }

        SkillDTO skill = SkillDTO.builder()
                .id(row.get("s_id", Long.class))
                .name(row.get("s_name", String.class))
                .type(SkillType.valueOf(row.get("type", String.class)))
                .build();

        if(existingTeam.getSkills().stream().noneMatch(s -> s.getId().equals(skill.getId()))) {
            existingTeam.getSkills().add(skill);
        }

        return existingTeam;
    }
}