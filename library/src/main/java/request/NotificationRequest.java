package request;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class NotificationRequest {
    private String notificationId;
    private String consumerEmail;
    private String tag;
    private String title;
    private String message;
    private String link;
    private String buttonName;
}
