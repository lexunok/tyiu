package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.TeamMarketRequestDTO;
import com.tyiu.corn.model.dto.TeamMemberDTO;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Row;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiFunction;

public class TeamMarketMapper implements BiFunction<Row, Object, TeamMarketRequestDTO> {

    private final Map<String, TeamMarketRequestDTO> teamMarketRequestDTOMap = new LinkedHashMap<>();

    @Override
    public TeamMarketRequestDTO apply(Row row, Object o) {
        String teamMarketRequestId = row.get("id", String.class);
        TeamMarketRequestDTO existingTeamMarketRequest = teamMarketRequestDTOMap.get(teamMarketRequestId);

        if (existingTeamMarketRequest == null)
        {
            existingTeamMarketRequest = TeamMarketRequestDTO.builder()
                    .id(teamMarketRequestId)
                    .ideaMarketId(row.get("idea_id", String.class))
                    .teamId(row.get("team_id", String.class))
                    .accepted(row.get("accepted", Boolean.class))
                    .name(row.get("name", String.class))
                    .updatedAt(row.get("updated_at", LocalDate.class))
                    .closed(row.get("closed", Boolean.class))
                    .description(row.get("description", String.class))
                    .owner(TeamMemberDTO.builder()
                            .userId(row.get("o_id", String.class))
                            .email(row.get("o_email", String.class))
                            .firstName(row.get("o_first_name", String.class))
                            .lastName(row.get("o_last_name", String.class))
                            .build())
                    .leader(TeamMemberDTO.builder()
                            .userId(row.get("l_id", String.class))
                            .email(row.get("l_email", String.class))
                            .firstName(row.get("l_first_name", String.class))
                            .lastName(row.get("l_last_name", String.class))
                            .build())
                    .members(new ArrayList<>())
                    .skills(new ArrayList<>())
                    .letter(row.get("letter", String.class))
                    .build();
            teamMarketRequestDTOMap.put(teamMarketRequestId, existingTeamMarketRequest);
        }

        TeamMemberDTO teamMemberDTO = TeamMemberDTO.builder()
                .userId(row.get("m_id", String.class))
                .email(row.get("m_email", String.class))
                .firstName(row.get("m_first_name", String.class))
                .lastName(row.get("m_last_name", String.class))
                .build();

        if(existingTeamMarketRequest.getMembers().stream().noneMatch(user -> user.getUserId().equals(teamMemberDTO.getUserId()))) {
            existingTeamMarketRequest.getMembers().add(teamMemberDTO);
        }

        SkillDTO skillDTO = SkillDTO.builder()
                .id(row.get("s_id", String.class))
                .name(row.get("s_name", String.class))
                .type(SkillType.valueOf(row.get("type", String.class)))
                .build();

        if(existingTeamMarketRequest.getSkills().stream().noneMatch(skill -> skill.getId().equals(skillDTO.getId()))) {
            existingTeamMarketRequest.getSkills().add(skillDTO);
        }

        return existingTeamMarketRequest;
    }
}
