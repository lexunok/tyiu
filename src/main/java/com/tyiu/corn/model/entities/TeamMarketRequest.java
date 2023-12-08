package com.tyiu.corn.model.entities;

import com.tyiu.corn.model.enums.RequestStatus;
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
public class TeamMarketRequest {
    @Id
    private String id;
    private String ideaMarketId;
    private String teamId;

    private String name;
    private RequestStatus status;
    private String letter;
}
