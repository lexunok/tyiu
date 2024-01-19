package com.tyiu.tgbotservice.model;

import lombok.Data;

@Data
public class NotificationTelegramResponse {

    private String tag;
    private String title;
    private String message;
    private String link;
}
