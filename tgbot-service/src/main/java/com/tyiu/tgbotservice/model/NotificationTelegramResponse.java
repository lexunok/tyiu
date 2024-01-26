package com.tyiu.tgbotservice.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class NotificationTelegramResponse {

    private String tag;
    private String title;
    private String message;
    private String link;
}
