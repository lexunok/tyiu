package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.NotificationDTO;
import com.tyiu.corn.service.NotificationService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;

@RestController
@RequestMapping("/api/v1/notification")
@RequiredArgsConstructor
public class NotificationController {

    private final NotificationService notificationService;

    @GetMapping("/all")
    public Flux<NotificationDTO> getAllNotifications(Principal principal) {
        return notificationService.getAllNotifications(principal.getName());
    }

    @GetMapping("/favourite")
    public Flux<NotificationDTO> getAllFavouriteNotifications(Principal principal) {
        return notificationService.getAllFavouriteNotifications(principal.getName());
    }

    @PostMapping("/create")
    public Mono<NotificationDTO> createNotification(@RequestBody NotificationDTO notification) {
        return notificationService.createNotification(notification)
                .switchIfEmpty(Mono.error(new NotFoundException("Create is not success!")));
    }

    @PutMapping("/favourite/{notificationId}")
    public Mono<Void> addNotificationToFavourite(@PathVariable String notificationId) {
        return notificationService.addNotificationToFavourite(notificationId);
    }

    @PutMapping("/unfavourite/{notificationId}")
    public Mono<Void> removeNotificationFromFavourite(@PathVariable String notificationId) {
        return notificationService.removeNotificationFromFavourite(notificationId);
    }

    @PutMapping("/show/{notificationId}")
    public Mono<Void> showNotification(@PathVariable String notificationId) {
        return notificationService.showNotification(notificationId);
    }

    @PutMapping("/read/{notificationId}")
    public Mono<Void> readNotification(@PathVariable String notificationId) {
        return notificationService.readNotification(notificationId);
    }
}
