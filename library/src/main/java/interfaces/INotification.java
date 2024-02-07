package interfaces;

import org.springframework.http.ResponseEntity;
import request.NotificationRequest;
import response.NotificationResponse;

public interface INotification {
    void makeNotification(NotificationRequest notificationRequest);
    void validateResponse(ResponseEntity<NotificationResponse> response);
}
