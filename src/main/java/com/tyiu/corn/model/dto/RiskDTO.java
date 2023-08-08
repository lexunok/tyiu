package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.enums.StatusIdea;

import lombok.Data;

@Data
public class RiskDTO {
    private StatusIdea status;
    private double risk;
    private String price;
    private String originality;
    private String technicalFeasibility;
    private String understanding;
}
