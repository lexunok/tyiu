package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDate;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class IdeaMarket {
    @Id
    private String id;
    private String ideaId;
    private String initiatorId;
    private Long position;
    private String name;
    private String description;
    private LocalDate createdAt;
    private Short maxTeamSize;
    private IdeaMarketStatusType status;
    private Long requests;
    private Long acceptedRequests;
    private LocalDate startDate;
    private LocalDate finishDate;
}
