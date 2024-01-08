package com.tyiu.corn.model.dto;

import com.tyiu.corn.model.enums.MarketStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MarketDTO {
    private String id;
    private String name;
    private LocalDate startDate;
    private LocalDate finishDate;
    private MarketStatus status;
}
