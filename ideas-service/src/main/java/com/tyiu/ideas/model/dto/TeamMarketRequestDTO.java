package com.tyiu.ideas.model.dto;

import com.tyiu.ideas.model.enums.RequestStatus;
import lombok.*;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamMarketRequestDTO {
    private String id;
    private String ideaMarketId;
    private String teamId;
    private String marketId;

    private String name;
    private RequestStatus status;
    private String letter;
    private Integer membersCount;
    private List<SkillDTO> skills;
}
