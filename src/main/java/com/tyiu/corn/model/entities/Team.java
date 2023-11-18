package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.dto.SkillDTO;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;
import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class Team {
    @Id
    private String id;
    private String name;
    private String description;

    private Boolean closed;
    private LocalDate createdAt;

    private String ownerId;
    private String leaderId;
    //private List<SkillDTO> desiredSkills;
}