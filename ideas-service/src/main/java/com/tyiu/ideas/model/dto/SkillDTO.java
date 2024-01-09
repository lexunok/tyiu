package com.tyiu.ideas.model.dto;

import com.tyiu.ideas.model.enums.SkillType;

import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SkillDTO {
    private String id;
    private String name;
    private SkillType type;
    private Boolean confirmed;
    private String creatorId;
    private String updaterId;
    private String deleterId;
}
