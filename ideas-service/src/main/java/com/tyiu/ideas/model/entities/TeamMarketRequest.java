package com.tyiu.ideas.model.entities;

import com.tyiu.ideas.model.enums.RequestStatus;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class TeamMarketRequest {
    @Id
    private String id;
    private String ideaMarketId;
    private String teamId;
    private String marketId;

    private String name;
    private RequestStatus status;
    private String letter;
}
