package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.ProfileDTO;
import io.r2dbc.spi.Row;
import org.springframework.stereotype.Component;

import java.util.function.BiFunction;
@Component
public class ProfileMapper implements BiFunction<Row,Object, ProfileDTO> {
    @Override
    public ProfileDTO apply(Row row, Object o) {
        return ProfileDTO.builder()
                .id(row.get("u_id",Long.class))
                .email(row.get("u_email",String.class))
                .lastName(row.get("u_last_name",String.class))
                .firstName(row.get("u_first_name",String.class))
                .build();
    }
}
