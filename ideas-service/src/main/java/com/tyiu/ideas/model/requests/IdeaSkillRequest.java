package com.tyiu.ideas.model.requests;

import com.tyiu.ideas.model.dto.SkillDTO;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class IdeaSkillRequest {
    private String ideaId;
    private List<SkillDTO> skills;
}
