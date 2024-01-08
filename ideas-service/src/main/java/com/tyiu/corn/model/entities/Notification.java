package com.tyiu.corn.model.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import java.time.LocalDateTime;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "notification")
public class Notification {
    @Id
    private String id;
    private String userId;
    private String title;
    private String message;
    private String link;
    private Boolean isShowed;
    private Boolean isReaded;
    private Boolean isFavourite;
    private LocalDateTime createdAt;
}
