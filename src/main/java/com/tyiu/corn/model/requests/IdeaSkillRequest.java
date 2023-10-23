package com.tyiu.corn.model.requests;

import com.tyiu.corn.model.dto.SkillDTO;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class IdeaSkillRequest {
    private Long ideaId;
    private List<SkillDTO> skills;
}
