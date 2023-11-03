package com.tyiu.corn.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class NotificationDTO {
    private Long id;
    private Long userId;
    private String title;
    private String message;
    private boolean isShowed;
    private boolean isReaded;
    private boolean isFavourite;
    private LocalDateTime createdAt;
}
