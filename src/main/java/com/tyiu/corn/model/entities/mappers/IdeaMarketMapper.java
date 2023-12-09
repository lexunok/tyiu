package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.IdeaMarketDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Row;

import java.time.LocalDate;
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
                    .description(row.get("description", String.class))
                    .problem(row.get("problem", String.class))
                    .result(row.get("result", String.class))
                    .customer(row.get("customer", String.class))
                    .solution(row.get("solution", String.class))
                    .stack(new ArrayList<>())
                    .createdAt(row.get("created_at", LocalDate.class))
                    .maxTeamSize(row.get("max_team_size", Short.class))
                    .status(IdeaMarketStatusType.valueOf(row.get("status", String.class)))
                    .requests(row.get("request_count", Integer.class))
                    .acceptedRequests(row.get("accepted_request_count", Integer.class))
                    .isFavorite(Objects.equals(row.get("idea_market_id", String.class), ideaMarketId))
                    .startDate(row.get("start_date", LocalDate.class))
                    .finishDate(row.get("finish_date", LocalDate.class))
                    .build();
            ideaMarketDTOMap.put(ideaMarketId, existingIdeaMarket);
        }

        String teamId = row.get("t_id", String.class);
        if (teamId != null){
            TeamDTO teamDTO = TeamDTO.builder()
                    .id(teamId)
                    .name(row.get("t_name", String.class))
                    .membersCount(row.get("member_count", Integer.class))
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
