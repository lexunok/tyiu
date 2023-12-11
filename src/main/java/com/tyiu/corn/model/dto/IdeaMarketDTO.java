package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.time.LocalDateTime;
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
    private LocalDate createdAt;
    private String name;
    private String problem;
    private String description;
    private String solution;
    private String result;
    private Short maxTeamSize;
    private String customer;
    private Long position;
    private List<SkillDTO> stack;
    private IdeaMarketStatusType status;
    private Integer requests;
    private Integer acceptedRequests;
    private Boolean isFavorite;
    private Boolean isRefused;
    private LocalDate startDate;
    private LocalDate finishDate;
}
