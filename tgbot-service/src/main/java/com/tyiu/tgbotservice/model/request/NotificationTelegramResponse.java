package com.tyiu.tgbotservice.model.request;

import lombok.*;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class NotificationTelegramResponse {

    private String tag;
    private String title;
    private String message;
    private String link;
}
