package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.CompanyDTO;
import com.tyiu.corn.model.dto.UserDTO;
import io.r2dbc.spi.Row;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.function.BiFunction;

@Component
public class CompanyMapper implements BiFunction<Row, Object, CompanyDTO> {

    @Override
    public CompanyDTO apply(Row row, Object o) {
        return CompanyDTO.builder()
                .id(row.get("id", String.class))
                .name(row.get("name", String.class))
                .owner(UserDTO.builder()
                        .id(row.get("owner_id", String.class))
                        .email(row.get("owner_email", String.class))
                        .firstName(row.get("owner_first_name", String.class))
                        .lastName(row.get("owner_last_name", String.class))
                        .build())
                .users(new ArrayList<>())
                .build();
    }
}
