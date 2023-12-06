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
    private Long position;
    private String name;
    private String description;
    private List<SkillDTO> stack;
    private LocalDateTime createdAt;
    private Short maxTeamSize;
    private IdeaMarketStatusType status;
    private Long requests;
    private Long acceptedRequests;
    private Boolean isFavorite;
    private LocalDate startDate;
    private LocalDate finishDate;
}
