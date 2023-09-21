package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.SkillType;

import java.time.Instant;

import lombok.*;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.annotation.Id;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Document
public class Skill {
    @Id
    private String id;
    private Instant createdAt;
    private String name;
    private SkillType type;
    private Boolean confirmed;
}
