package com.tyiu.ideas.model.dto;

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
