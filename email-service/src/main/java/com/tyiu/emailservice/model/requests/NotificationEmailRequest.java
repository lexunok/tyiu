package com.tyiu.emailservice.model.requests;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class NotificationEmailRequest {
    private String consumerEmail;
    private String title;
    private String message;
    private String link;
    private String buttonName;
}
