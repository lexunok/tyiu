package interfaces;

import request.NotificationRequest;
import response.ResponseEntity;
import response.NotificationResponse;

public interface INotification {
    void makeNotification(NotificationRequest notificationRequest);
    void validateResponse(ResponseEntity<NotificationResponse> response);
}
