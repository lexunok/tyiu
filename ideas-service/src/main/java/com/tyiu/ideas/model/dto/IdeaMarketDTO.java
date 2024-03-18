package com.tyiu.ideas.model.dto;

import com.tyiu.ideas.model.enums.IdeaMarketStatusType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class IdeaMarketDTO {
    private String id;
    private String ideaId;
    private UserDTO initiator;
    private TeamDTO team;
    private String marketId;
    private String name;
    private String problem;
    private String description;
    private String solution;
    private String result;
    private Short maxTeamSize;
    private String customer;
    private Integer position;
    private List<SkillDTO> stack;
    private IdeaMarketStatusType status;
    private Integer requests;
    private Integer acceptedRequests;
    private Boolean isFavorite;

    public String getIdeaId() { return ideaId; }
    public TeamDTO getTeam() { return team; }
    public String getMarketId() { return marketId; }

}
