package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.enums.RequestStatus;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TeamMarketRequestDTO {
    private String id;
    private String ideaMarketId;
    private String teamId;

    private String name;
    private RequestStatus status;
    private String letter;
    private Integer membersCount;
    private List<SkillDTO> skills;
}
