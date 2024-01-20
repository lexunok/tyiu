package com.tyiu.ideas.model.entities.mappers;

import com.tyiu.ideas.model.dto.ProfileDTO;
import com.tyiu.ideas.model.enums.Role;
import io.r2dbc.spi.Row;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.function.BiFunction;
@Component
public class ProfileMapper implements BiFunction<Row,Object, ProfileDTO> {
    @Override
    public ProfileDTO apply(Row row, Object o) {
        return ProfileDTO.builder()
                .id(row.get("u_id",String.class))
                .email(row.get("u_email",String.class))
                .lastName(row.get("u_last_name",String.class))
                .firstName(row.get("u_first_name",String.class))
                .roles(Arrays.stream(row.get("u_roles",String[].class)).map(Role::valueOf).toList())
                .isUserTagVisible(row.get("t_is_visible", Boolean.class))
                .createdAt(row.get("u_created_at", LocalDateTime.class))
                .build();
    }
}
