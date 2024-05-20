package com.tyiu.ideas.model.entities.mappers;

import com.tyiu.client.models.Role;
import com.tyiu.ideas.model.dto.GroupDTO;
import io.r2dbc.spi.Row;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.function.BiFunction;
@Component
public class GroupMapper implements BiFunction<Row, Object, GroupDTO> {

    @Override
    public GroupDTO apply(Row row, Object o) {
        return GroupDTO.builder()
                    .id(row.get("id", String.class))
                    .name(row.get("name", String.class))
                    .roles(Arrays.stream(row.get("roles",String[].class))
                            .map(Role::valueOf).toList())
                    .users(new ArrayList<>())
                    .build();
    }
}