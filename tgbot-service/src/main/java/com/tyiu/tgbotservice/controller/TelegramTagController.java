package com.tyiu.tgbotservice.controller;

import com.tyiu.tgbotservice.model.entities.UserTelegram;
import com.tyiu.tgbotservice.service.TelegramTagService;

import reactor.core.publisher.Mono;
import org.springframework.security.oauth2.jwt.Jwt;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.security.core.annotation.AuthenticationPrincipal;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/tgbot-service/telegram")
public class TelegramTagController {

    private final TelegramTagService telegramTagService;

    @PostMapping("/add-tag")
    public Mono<Void> addTelegramTag(@RequestBody UserTelegram userTelegram,
                                     @AuthenticationPrincipal Jwt jwt) {
        return telegramTagService.addTelegramTag(userTelegram, jwt.getClaimAsString("sub"));
    }

    @PutMapping("/update/{userTag}")
    public Mono<Void> updateTelegramTag(@PathVariable String userTag,
                                        @AuthenticationPrincipal Jwt jwt) {
        return telegramTagService.updateTelegramTag(userTag, jwt.getClaimAsString("sub")).then();
    }

    @PutMapping("/visibility/{userTag}")
    private Mono<Void> setTelegramTagVisibility(@RequestBody UserTelegram userTelegram,
                                              @AuthenticationPrincipal Jwt jwt) {
        return telegramTagService.setTelegramTagVisibility(userTelegram, jwt.getClaimAsString("sub"));
    }

    @DeleteMapping("/delete/{userTag}")
    public Mono<Void> deleteTelegramInfo(@PathVariable String userTag,
                                         @AuthenticationPrincipal Jwt jwt) {
        return telegramTagService.deleteTelegramInfo(userTag, jwt.getClaimAsString("sub"));
    }
}
