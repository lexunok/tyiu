package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document
public class Rating {
    @Id
    private String id;
    private String ideaId;
    private String expert;

    private Long marketValue;
    private Long originality;
    private Long technicalRealizability;
    private Long suitability;
    private Long budget;
    private double rating;
    private Boolean confirmed;
}
