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
