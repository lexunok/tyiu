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
@Table
public class Notification {
    @Id
    private Long id;
    private Long userId;
    private String title;
    private String message;
    private boolean isShowed;
    private boolean isReaded;
    private boolean isFavourite;
    private LocalDateTime createdAt;
}
