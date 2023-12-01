package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.*;
import io.r2dbc.spi.Row;

import java.time.LocalDate;
import java.util.*;


public class TeamMapper {
    private final Map<String, TeamDTO> teamDTOMap = new LinkedHashMap<>();

    public TeamDTO apply(Row row, Object o) {
        String teamId = row.get("team_id", String.class);
        TeamDTO existingTeam = teamDTOMap.get(teamId);

        if (existingTeam == null) {
            existingTeam = new TeamDTO();
            existingTeam.setId(teamId);
            existingTeam.setName(row.get("team_name", String.class));
            existingTeam.setDescription(row.get("team_description", String.class));
            existingTeam.setClosed(row.get("team_closed", Boolean.class));
            existingTeam.setCreatedAt(row.get("team_created_at", LocalDate.class));

            existingTeam.setOwner(UserDTO.builder().id(row.get("owner_id", String.class)).build());
            existingTeam.setLeader(UserDTO.builder().id(row.get("leader_id", String.class)).build());

            existingTeam.setMembers(new ArrayList<>());
            existingTeam.setTotalSkills(new ArrayList<>());

            teamDTOMap.put(teamId, existingTeam);
        }

        String memberId = row.get("member_id", String.class);

        if (memberId != null) {
            UserDTO member = new UserDTO();
            member.setId(memberId);
            member.setEmail(row.get("member_email", String.class));
            member.setFirstName(row.get("member_first_name", String.class));
            member.setLastName(row.get("member_last_name", String.class));

            if (existingTeam.getMembers().stream().noneMatch(m -> m.getId().equals(member.getId()))) {
                existingTeam.getMembers().add(member);
                existingTeam.setMembersCount(existingTeam.getMembers().size());
            }
        }

        String skillId = row.get("skill_id", String.class);

        if (skillId != null) {
            existingTeam.getTotalSkills().add(skillId);
        }

        return existingTeam;
    }


}