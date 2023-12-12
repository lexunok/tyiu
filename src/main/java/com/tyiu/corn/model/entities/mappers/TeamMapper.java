package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Row;
import com.tyiu.corn.model.dto.TeamDTO;
import java.time.LocalDate;
import java.util.*;

public class TeamMapper {
    private final Map<String, TeamDTO> teamDTOMap = new LinkedHashMap<>();

    public TeamDTO apply(Row row, Object o) {
        String teamId = row.get("team_id", String.class);
        TeamDTO existingTeam = teamDTOMap.get(teamId);

        if (existingTeam == null) {
            existingTeam = TeamDTO.builder()
                    .id(teamId)
                    .name(row.get("team_name", String.class))
                    .description(row.get("team_description", String.class))
                    .closed(row.get("team_closed", Boolean.class))
                    .hasActiveProject(row.get("team_has_active_project", Boolean.class))
                    .createdAt(row.get("team_created_at", LocalDate.class))
                    .membersCount(row.get("member_count", Integer.class))
                    .owner(UserDTO.builder()
                            .id(row.get("owner_id", String.class))
                            .email(row.get("owner_email", String.class))
                            .firstName(row.get("owner_first_name", String.class))
                            .lastName(row.get("owner_last_name", String.class))
                            .build())
                    .isRefused(Objects.equals(row.get("refused_team_id", String.class), teamId) || row.get("existed_member", String.class) != null)
                    .members(new ArrayList<>())
                    .skills(new ArrayList<>())
                    .wantedSkills(new ArrayList<>())
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

        if (memberId != null) {
            UserDTO member = UserDTO.builder()
                    .id(memberId)
                    .email(row.get("member_email", String.class))
                    .firstName(row.get("member_first_name", String.class))
                    .lastName(row.get("member_last_name", String.class))
                    .build();

            if(existingTeam.getMembers().stream().noneMatch(m -> m.getId().equals(member.getId()))) {
                existingTeam.getMembers().add(member);
            }
        }

        String skillId = row.get("skill_id", String.class);

        if (skillId != null) {
            SkillDTO skill = SkillDTO.builder()
                    .id(skillId)
                    .name(row.get("skill_name", String.class))
                    .type(SkillType.valueOf(row.get("skill_type", String.class)))
                    .build();

            if(existingTeam.getSkills().stream().noneMatch(s -> s.getId().equals(skill.getId()))) {
                existingTeam.getSkills().add(skill);
            }
        }

        String wantedSkillId = row.get("wanted_skill_id", String.class);

        if (wantedSkillId != null) {
            SkillDTO skill = SkillDTO.builder()
                    .id(wantedSkillId)
                    .name(row.get("wanted_skill_name", String.class))
                    .type(SkillType.valueOf(row.get("wanted_skill_type", String.class)))
                    .build();

            if(existingTeam.getWantedSkills().stream().noneMatch(s -> s.getId().equals(skill.getId()))) {
                existingTeam.getWantedSkills().add(skill);
            }
        }

        return existingTeam;
    }
}