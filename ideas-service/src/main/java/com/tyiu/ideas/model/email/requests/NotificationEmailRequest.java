package com.tyiu.ideas.model.email.requests;

import lombok.*;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NotificationEmailRequest {
    private String title;
    private String to;
    private String message;
    private String buttonName;
    private String link;
}
