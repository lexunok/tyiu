package com.tyiu.corn.model.email.requests;

import lombok.*;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NotificationEmailRequest {
    private String title;
    private String to;
    private String from;
    private String message;
    private String link;
}
