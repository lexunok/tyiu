package com.tyiu.ideas.model.entities;

import com.tyiu.ideas.model.enums.IdeaMarketStatusType;
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
    private String teamId;
    private String marketId;
    private IdeaMarketStatusType status;
}
