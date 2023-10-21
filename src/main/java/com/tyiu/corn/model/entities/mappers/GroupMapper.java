package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.enums.Role;
import io.r2dbc.spi.Row;

import java.util.*;
import java.util.function.BiFunction;

public class GroupMapper implements BiFunction<Row, Object, GroupDTO> {

    private final Map<Long, GroupDTO> groupDTOMap = new LinkedHashMap<>();

    @Override
    public GroupDTO apply(Row row, Object o) {
        UserDTO userDTO = UserDTO.builder()
                .id(row.get("member_id", Long.class))
                .email(row.get("email", String.class))
                .firstName(row.get("first_name", String.class))
                .lastName(row.get("last_name", String.class))
                .build();

        Long groupId = row.get("id", Long.class);
        GroupDTO existingGroup = groupDTOMap.get(groupId);

        if (existingGroup == null) {
            existingGroup = GroupDTO.builder()
                    .id(groupId)
                    .name(row.get("name", String.class))
                    .roles(Arrays.stream(row.get("roles", String[].class))
                            .map(Role::valueOf)
                            .toList())
                    .users(new ArrayList<>())
                    .build();
            groupDTOMap.put(groupId, existingGroup);
        }

        if(existingGroup.getUsers().stream().noneMatch(user -> user.getId().equals(userDTO.getId()))) {
            existingGroup.getUsers().add(userDTO);
        }

        return existingGroup;
    }
}