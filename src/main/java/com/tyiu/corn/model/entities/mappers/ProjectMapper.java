package com.tyiu.corn.model.entities.mappers;

import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.enums.SkillType;
import io.r2dbc.spi.Row;

import java.time.LocalDate;
import java.util.*;
import java.util.function.BiFunction;

public class ProjectMapper implements BiFunction<Row, Object, ProjectDTO> {

    private final Map<String, ProjectDTO> projectDTOMap = new LinkedHashMap<>();

    @Override
    public ProjectDTO apply(Row row, Object o) {
        String projectId = row.get("id", String.class);
        ProjectDTO existingProject = projectDTOMap.get(projectId);

        if (existingProject == null) {
            existingProject = ProjectDTO.builder()
                    .id(projectId)
                    .name(row.get("name", String.class))
                    .description(row.get("description", String.class))
                    .team(TeamDTO.builder()
                            .id(row.get("t_id", String.class))
                            .name(row.get("t_name", String.class))
                            .description(row.get("t_description", String.class))
                            .closed(row.get("closed", Boolean.class))
                            .createdAt(row.get("created_at", LocalDate.class))
                            .owner(UserDTO.builder()
                                    .id(row.get("o_id", String.class))
                                    .email(row.get("o_email", String.class))
                                    .firstName(row.get("o_first_name", String.class))
                                    .lastName(row.get("o_last_name", String.class))
                                    .build())
                            .leader(UserDTO.builder()
                                    .id(row.get("l_id", String.class))
                                    .email(row.get("l_email", String.class))
                                    .firstName(row.get("l_first_name", String.class))
                                    .lastName(row.get("l_last_name", String.class))
                                    .build())
                            .members(new ArrayList<>())
                            .skills(new ArrayList<>())
                            .build())
                    .build();
            projectDTOMap.put(projectId, existingProject);
        }

        UserDTO member = UserDTO.builder()
                .id(row.get("m_id", String.class))
                .email(row.get("m_email", String.class))
                .firstName(row.get("m_first_name", String.class))
                .lastName(row.get("m_last_name", String.class))
                .build();

        if(existingProject.getTeam().getMembers().stream().noneMatch(m -> m.getId().equals(member.getId()))) {
            existingProject.getTeam().getMembers().add(member);
            existingProject.setMembersCount(existingProject.getTeam().getMembers().size());
        }

        SkillDTO skill = SkillDTO.builder()
                .id(row.get("s_id", String.class))
                .name(row.get("s_name", String.class))
                .type(SkillType.valueOf(row.get("type", String.class)))
                .build();

        if(existingProject.getTeam().getSkills().stream().noneMatch(s -> s.getId().equals(skill.getId()))) {
            existingProject.getTeam().getSkills().add(skill);
        }

        return existingProject;
    }
}
