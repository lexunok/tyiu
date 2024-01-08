package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.RatingDTO;
import io.r2dbc.spi.Row;
import org.springframework.stereotype.Component;
import java.util.function.BiFunction;

@Component
public class RatingMapper implements BiFunction<Row, Object, RatingDTO> {

    @Override
    public RatingDTO apply(Row row, Object o) {
        return RatingDTO.builder()
                .id(row.get("id", String.class))
                .ideaId(row.get("idea_id",String.class))
                .expertId(row.get("expert_id",String.class))
                .expertFirstName(row.get("expert_first_name",String.class))
                .expertLastName(row.get("expert_last_name",String.class))
                .marketValue(row.get("market_value", Long.class))
                .originality(row.get("originality", Long.class))
                .technicalRealizability(row.get("technical_realizability", Long.class))
                .suitability(row.get("suitability", Long.class))
                .budget(row.get("budget", Long.class))
                .rating(row.get("rating", Double.class))
                .isConfirmed(row.get("is_confirmed", Boolean.class))
                .build();
    }
}