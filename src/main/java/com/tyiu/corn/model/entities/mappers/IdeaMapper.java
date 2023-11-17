package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.enums.ProjectType;
import com.tyiu.corn.model.enums.StatusIdea;
import io.r2dbc.spi.Row;

import java.time.LocalDateTime;
import java.util.function.BiFunction;

public class IdeaMapper implements BiFunction<Row, Object, IdeaDTO> {
    @Override
    public IdeaDTO apply(Row row, Object o) {
        GroupDTO experts = GroupDTO.builder()
                .id(row.get("experts_id", String.class))
                .name(row.get("experts_name", String.class)).build();
        GroupDTO projectOffice = GroupDTO.builder()
                .id(row.get("project_office_id", String.class))
                .name(row.get("project_office_name", String.class)).build();
        return IdeaDTO.builder()
                .id(row.get("id", String.class))
                .initiatorEmail(row.get("initiator_email", String.class))
                .name(row.get("name", String.class))
                .experts(experts)
                .projectOffice(projectOffice)
                .status(StatusIdea.valueOf(row.get("status", String.class)))
                .createdAt(row.get("created_at", LocalDateTime.class))
                .modifiedAt(row.get("modified_at", LocalDateTime.class))
                .maxTeamSize(row.get("max_team_size", Short.class))
                .minTeamSize(row.get("min_team_size", Short.class))
                .problem(row.get("problem", String.class))
                .solution(row.get("solution", String.class))
                .result(row.get("result", String.class))
                .customer(row.get("customer", String.class))
                .contactPerson(row.get("contact_person", String.class))
                .description(row.get("description", String.class))
                .suitability(row.get("suitability", Long.class))
                .budget(row.get("budget", Long.class))
                .preAssessment(row.get("pre_assessment", Double.class))
                .rating(row.get("rating", Double.class))
                .build();
    }
}
