package interfaces;

import org.springframework.http.ResponseEntity;
import requests.NotificationRequest;
import response.NotificationResponse;

public interface INotificationRabbitMQ {
    void makeNotification(NotificationRequest notificationRequest);
    void validateResponse(ResponseEntity<NotificationResponse> response);
}
