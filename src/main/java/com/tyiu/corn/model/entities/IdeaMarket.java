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
    private LocalDate createdAt;
    private String name;
    private String problem;
    private String description;
    private String solution;
    private String result;
    private Short maxTeamSize;
    private String customer;
    private IdeaMarketStatusType status;
    private LocalDate startDate;
    private LocalDate finishDate;
}
