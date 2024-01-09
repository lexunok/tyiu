package com.tyiu.ideas.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Builder
@Table
@Setter
public class Rating {
    @Id
    private String id;
    private String ideaId;
    private String expertId;
    private Long marketValue;
    private Long originality;
    private Long technicalRealizability;
    private Long suitability;
    private Long budget;
    private Double rating;
    private Boolean isConfirmed;
}
