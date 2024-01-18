package com.tyiu.emailservice.model.responses;

import lombok.*;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NotificationEmailResponse {
    private String title;
    private String to;
    private String message;
    private String buttonName;
    private String link;
}
