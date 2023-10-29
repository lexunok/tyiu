package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.IdeaMarketDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Row;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiFunction;

public class IdeaMarketMapper implements BiFunction<Row, Object, IdeaMarketDTO> {

    private final Map<Long, IdeaMarketDTO> ideaMarketDTOMap = new LinkedHashMap<>();

    //@Override
    public IdeaMarketDTO apply(Row row, Object o) {
        Long ideaMarketId = row.get("id", Long.class);
        IdeaMarketDTO existingIdeaMarket = ideaMarketDTOMap.get(ideaMarketId);

        if (existingIdeaMarket == null)
        {
            existingIdeaMarket = IdeaMarketDTO.builder()
                    .id(ideaMarketId)
                    .position(row.get("position", Long.class))
                    .name(row.get("name", String.class))
                    .initiator(row.get("initiator", String.class))
                    .description(row.get("description", String.class))
                    .stack(new ArrayList<>())
                    .createdAt(row.get("created_at", LocalDate.class))
                    .maxTeamSize(row.get("max_team_size", Long.class))
                    .status(IdeaMarketStatusType.valueOf(row.get("status", String.class)))
                    .requests(row.get("requests", Long.class))
                    .acceptedRequests(row.get("accepted_requests", Long.class))
                    .isFavorite(false)
                    .build();
            ideaMarketDTOMap.put(ideaMarketId, existingIdeaMarket);
        }

        SkillDTO skillDTO = SkillDTO.builder()
                .id(row.get("s_id", Long.class))
                .name(row.get("s_name", String.class))
                .type(SkillType.valueOf(row.get("type", String.class)))
                .build();

        if (row.get("idea_market_id", Long.class) == ideaMarketId)
        {
            existingIdeaMarket.setIsFavorite(true);
        }

        if(existingIdeaMarket.getStack().stream().noneMatch(skill -> skill.getId().equals(skillDTO.getId()))) {
            existingIdeaMarket.getStack().add(skillDTO);
        }

        return existingIdeaMarket;
    }
}
