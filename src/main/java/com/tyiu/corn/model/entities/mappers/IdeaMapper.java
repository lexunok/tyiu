package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.entities.Group;
import io.r2dbc.spi.Row;
import java.util.function.BiFunction;

public class IdeaMapper implements BiFunction<Row, Object, IdeaDTO> {
    @Override
    public IdeaDTO apply(Row row, Object o) {
        Group experts = Group.builder()
                .id(row.get("group_id",Long.class))
                .name(row.get("group_name", String.class)).build();
        return IdeaDTO.builder()
                .id(row.get("id",Long.class))
                .createdAt(row.get())
                .budget(row.get())
                .description(row.get())
                .initiator(row.get("initiator", String.class))
                .modifiedAt(row.get())
                .name(row.get("name", String.class))
                .experts(experts).build();
    }
}
