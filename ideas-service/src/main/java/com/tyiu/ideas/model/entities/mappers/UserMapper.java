package com.tyiu.ideas.model.entities.mappers;

import com.tyiu.ideas.model.dto.UserDTO;
import io.r2dbc.spi.Row;
import org.springframework.stereotype.Component;

import java.util.function.BiFunction;
@Component
public class UserMapper implements BiFunction<Row, Object, UserDTO> {

    @Override
    public UserDTO apply(Row row, Object o) {
        return UserDTO.builder()
                .id(row.get("user_id", String.class))
                .email(row.get("email", String.class))
                .firstName(row.get("first_name", String.class))
                .lastName(row.get("last_name", String.class))
                .build();
    }
}