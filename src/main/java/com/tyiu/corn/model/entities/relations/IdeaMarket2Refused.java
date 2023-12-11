package com.tyiu.corn.model.entities.relations;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Table;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "idea_market_refused")
public class IdeaMarket2Refused {
    private String ideaMarketId;
    private String teamId;
}
