package com.tyiu.tgbotservice.controller;

import com.tyiu.tgbotservice.model.entities.UserTelegram;
import com.tyiu.tgbotservice.service.TelegramTagService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/profile/telegram")
@RequiredArgsConstructor
public class TelegramTagController {

    private final TelegramTagService telegramTagService;

//    @PostMapping("/add-tag")
//    public Mono<Void> addTelegramTag(@RequestBody UserTelegram userTelegram,
//                                     @AuthenticationPrincipal User user) {
//        return telegramTagService.addTelegramTag(userTelegram, user.getEmail());
//    }

    @PostMapping("/add-tag")
    public Mono<Void> addTelegramTag(@RequestBody UserTelegram userTelegram) {
        return telegramTagService.addTelegramTag(userTelegram, userTelegram.getUserEmail());
    }

//    @PutMapping("/update/{userTag}")
//    public Mono<Void> updateTelegramTag(@PathVariable String userTag,
//                                        @AuthenticationPrincipal User user) {
//        return telegramTagService.updateTelegramTag(userTag, user.getEmail()).then();
//    }
//
//    @PutMapping("/visibility/{userTag}")
//    private Mono<Void> setTelegramTagVisibility(@RequestBody UserTelegram userTelegram,
//                                              @AuthenticationPrincipal User user) {
//        return telegramTagService.setTelegramTagVisibility(userTelegram, user.getEmail());
//    }
//
//    @DeleteMapping("/delete/{userTag}")
//    public Mono<Void> deleteTelegramInfo(@PathVariable String userTag,
//                                         @AuthenticationPrincipal User user) {
//        return telegramTagService.deleteTelegramInfo(userTag, user.getEmail());
//    }
}
