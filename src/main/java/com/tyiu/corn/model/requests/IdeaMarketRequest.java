package com.tyiu.corn.model.requests;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
public class IdeaMarketRequest {
    private String id;
    private String initiatorEmail;
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
