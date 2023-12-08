package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.IdeaMarketDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Row;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;

public class IdeaMarketMapper implements BiFunction<Row, Object, IdeaMarketDTO> {

    private final Map<String, IdeaMarketDTO> ideaMarketDTOMap = new LinkedHashMap<>();

    //@Override
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
                    .isFavorite(false)
                    .startDate(row.get("start_date", LocalDate.class))
                    .finishDate(row.get("finish_date", LocalDate.class))
                    .build();
            ideaMarketDTOMap.put(ideaMarketId, existingIdeaMarket);
        }


        if (Objects.equals(row.get("idea_market_id", String.class), ideaMarketId)) {
            existingIdeaMarket.setIsFavorite(true);
        }

        String skillId = row.get("s_id", String.class);
        if (skillId != null) {
            SkillDTO skillDTO = SkillDTO.builder()
                    .id(skillId)
                    .name(row.get("s_name", String.class))
                    .type(SkillType.valueOf(row.get("type", String.class)))
                    .build();


            if (existingIdeaMarket.getStack().stream().noneMatch(skill -> skill.getId().equals(skillDTO.getId()))) {
                existingIdeaMarket.getStack().add(skillDTO);
            }
        }



        return existingIdeaMarket;
    }
}
