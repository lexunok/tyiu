package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Row;

import java.time.LocalDate;
import java.util.*;
import java.util.function.BiFunction;

public class TeamMapper implements BiFunction<Row, Object, TeamDTO> {

    private final Map<String, TeamDTO> teamDTOMap = new LinkedHashMap<>();

    @Override
    public TeamDTO apply(Row row, Object o) {
        String teamId = row.get("team_id", String.class);
        TeamDTO existingTeam = teamDTOMap.get(teamId);

        if (existingTeam == null) {
            existingTeam = TeamDTO.builder()
                    .id(teamId)
                    .name(row.get("team_name", String.class))
                    .description(row.get("team_description", String.class))
                    .closed(row.get("team_closed", Boolean.class))
                    .createdAt(row.get("team_created_at", LocalDate.class))
                    .owner(UserDTO.builder()
                            .id(row.get("owner_id", String.class))
                            .email(row.get("owner_email", String.class))
                            .firstName(row.get("owner_first_name", String.class))
                            .lastName(row.get("owner_last_name", String.class))
                            .build())
                    .members(new ArrayList<>())
                    .skills(new ArrayList<>())
                    .desiredSkills(new ArrayList<>())
                    .build();
            
            if (row.get("leader_id", String.class) != null) {
                existingTeam.setLeader(UserDTO.builder()
                            .id(row.get("leader_id", String.class))
                            .email(row.get("leader_email", String.class))
                            .firstName(row.get("leader_first_name", String.class))
                            .lastName(row.get("leader_last_name", String.class))
                            .build());
            }

            teamDTOMap.put(teamId, existingTeam);
        }

        String memberId = row.get("member_id", String.class);

        if (memberId != null){
            UserDTO member = UserDTO.builder()
                .id(memberId)
                .email(row.get("member_email", String.class))
                .firstName(row.get("member_first_name", String.class))
                .lastName(row.get("member_last_name", String.class))
                .build();
            if(existingTeam.getMembers().stream().noneMatch(m -> m.getId().equals(member.getId()))) {
                existingTeam.getMembers().add(member);
                existingTeam.setMembersCount(existingTeam.getMembers().size());
            }
        }

        String skillId = row.get("skill_id", String.class);

        if (skillId != null){
            SkillDTO skill = SkillDTO.builder()
                    .id(skillId)
                    .name(row.get("skill_name", String.class))
                    .build();

            if(existingTeam.getSkills().stream().noneMatch(s -> s.getId().equals(skill.getId()))) {
                existingTeam.getSkills().add(skill);
            }
        }
        return existingTeam;
    }
}