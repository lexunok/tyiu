package com.tyiu.ideas.model.entities.mappers;

import com.tyiu.ideas.model.dto.IdeaMarketDTO;
import com.tyiu.ideas.model.dto.SkillDTO;
import com.tyiu.ideas.model.dto.TeamDTO;
import com.tyiu.ideas.model.dto.UserDTO;
import com.tyiu.ideas.model.enums.IdeaMarketStatusType;
import com.tyiu.ideas.model.enums.SkillType;
import io.r2dbc.spi.Row;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;

public class IdeaMarketMapper implements BiFunction<Row, Object, IdeaMarketDTO> {

    private final Map<String, IdeaMarketDTO> ideaMarketDTOMap = new LinkedHashMap<>();

    public IdeaMarketDTO apply(Row row, Object o) {
        String ideaMarketId = row.get("id", String.class);
        IdeaMarketDTO existingIdeaMarket = ideaMarketDTOMap.get(ideaMarketId);

        if (existingIdeaMarket == null) {
            existingIdeaMarket = IdeaMarketDTO.builder()
                    .id(ideaMarketId)
                    .ideaId(row.get("idea_id", String.class))
                    .name(row.get("name", String.class))
                    .initiator(UserDTO.builder()
                            .id(row.get("u_id", String.class))
                            .email(row.get("u_e", String.class))
                            .firstName(row.get("u_fn", String.class))
                            .lastName(row.get("u_ln", String.class))
                            .build())
                    .marketId(row.get("market_id", String.class))
                    .description(row.get("description", String.class))
                    .problem(row.get("problem", String.class))
                    .result(row.get("result", String.class))
                    .customer(row.get("customer", String.class))
                    .solution(row.get("solution", String.class))
                    .stack(new ArrayList<>())
                    .maxTeamSize(row.get("max_team_size", Short.class))
                    .status(IdeaMarketStatusType.valueOf(row.get("status", String.class)))
                    .requests(row.get("request_count", Integer.class))
                    .acceptedRequests(row.get("accepted_request_count", Integer.class))
                    .isFavorite(Objects.equals(row.get("idea_market_id", String.class), ideaMarketId))
                    .build();
            ideaMarketDTOMap.put(ideaMarketId, existingIdeaMarket);
        }

        String teamId = row.get("t_id", String.class);
        if (teamId != null && existingIdeaMarket.getTeam() == null){
            TeamDTO teamDTO = TeamDTO.builder()
                    .id(teamId)
                    .name(row.get("t_name", String.class))
                    .membersCount(row.get("member_count", Integer.class))
                    .owner(UserDTO.builder()
                            .id(row.get("o_id", String.class))
                            .email(row.get("o_e", String.class))
                            .firstName(row.get("o_fn", String.class))
                            .lastName(row.get("o_ln", String.class))
                            .build())
                    .leader(UserDTO.builder()
                            .id(row.get("l_id", String.class))
                            .email(row.get("l_e", String.class))
                            .firstName(row.get("l_fn", String.class))
                            .lastName(row.get("l_ln", String.class))
                            .build())
                    .skills(new ArrayList<>())
                    .build();
            existingIdeaMarket.setTeam(teamDTO);
        }

        String ideaSkillId = row.get("si_id", String.class);
        if (ideaSkillId != null) {
            SkillDTO ideaSkillDTO = SkillDTO.builder()
                    .id(ideaSkillId)
                    .name(row.get("si_name", String.class))
                    .type(SkillType.valueOf(row.get("si_type", String.class)))
                    .build();

            if (existingIdeaMarket.getStack().stream().noneMatch(skill -> skill.getId().equals(ideaSkillDTO.getId()))) {
                existingIdeaMarket.getStack().add(ideaSkillDTO);
            }
        }

        String teamSkillId = row.get("st_id", String.class);
        if (teamSkillId != null) {
            SkillDTO teamSkillDTO = SkillDTO.builder()
                    .id(teamSkillId)
                    .name(row.get("st_name", String.class))
                    .type(SkillType.valueOf(row.get("st_type", String.class)))
                    .build();

            if (existingIdeaMarket.getTeam().getSkills().stream().noneMatch(skill -> skill.getId().equals(teamSkillDTO.getId()))) {
                existingIdeaMarket.getTeam().getSkills().add(teamSkillDTO);
            }
        }

        return existingIdeaMarket;
    }
}
