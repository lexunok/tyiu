package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.enums.SkillType;

import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SkillDTO {
    private Long id;
    private String name;
    private SkillType type;
    private Boolean confirmed;
    private Long creatorId;
    private Long updaterId;
    private Long deleterId;
}
