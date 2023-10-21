package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;


@Getter
@Builder
@Table
public class Rating {
    @Id
    private Long id;
    private Long ideaId;
    private Long expertId;
    private Long marketValue;
    private Long originality;
    private Long technicalRealizability;
    private Long suitability;
    private Long budget;
    private Double rating;
    private Boolean confirmed;
}
