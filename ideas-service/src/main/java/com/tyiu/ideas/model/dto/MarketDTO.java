package com.tyiu.ideas.model.dto;

import com.tyiu.ideas.model.enums.MarketStatus;
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

    public MarketDTO(String id, LocalDate finishDate) {
        this.id = id;
        this.finishDate = finishDate;
    }
}
