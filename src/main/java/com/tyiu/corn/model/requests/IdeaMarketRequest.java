package com.tyiu.corn.model.requests;

import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.enums.StatusIdea;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class IdeaMarketRequest {
    private String id;
    private String initiator;
    private LocalDateTime createdAt;
    private String name;
    private String problem;
    private String description;
    private String solution;
    private String result;
    private Short maxTeamSize;
    private String customer;
    private Long position;
    private LocalDate startDate;
    private LocalDate finishDate;
}
