package com.tyiu.corn.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RatingDTO {
    private String id;
    private String ideaId;
    private String expertId;
    private String expertFirstName;
    private String expertLastName;
    private Long marketValue;
    private Long originality;
    private Long technicalRealizability;
    private Long suitability;
    private Long budget;
    private Double rating;
    private Boolean isConfirmed;
}
