package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDateTime;
import java.util.List;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table
public class IdeaMarketAdvertisement {
    @Id
    private String id;
    private String ideaMarketId;
    private LocalDateTime createdAt;

    private String text;
    private String senderId;
    private List<String> checkedBy;
}