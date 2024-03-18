package com.tyiu.ideas.model.entities;

import com.tyiu.ideas.model.enums.MarketStatus;
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
public class Market {
    @Id
    private String id;
    private String name;
    private LocalDate startDate;
    private LocalDate finishDate;
    private MarketStatus status;

    public String getId() { return id; }
    public LocalDate getFinishDate() { return finishDate; }
}
