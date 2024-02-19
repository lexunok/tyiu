package response;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Getter
@Setter
public class NotificationResponse {
    private String notificationId;
    private String message;
}
