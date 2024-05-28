package com.tyiu.ideas.controller;

import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.ideas.model.dto.MarketDTO;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.enums.MarketStatus;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.MarketService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/ideas-service/market")
public class MarketController {

    private final MarketService marketService;


    @GetMapping("/all")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE', 'TEACHER', 'ADMIN')")
    public Flux<MarketDTO> getAll(){
        return marketService.getAll();
    }

    @GetMapping("/active")
    @PreAuthorize("hasAnyRole('MEMBER', 'TEACHER', 'INITIATOR', 'TEAM_OWNER', 'PROJECT_OFFICE', 'ADMIN')")
    public Flux<MarketDTO> getActiveMarket(){
        return marketService.getActiveMarkets();
    }

    @GetMapping("/{marketId}")
    @PreAuthorize("hasAnyRole('MEMBER', 'TEACHER', 'INITIATOR', 'TEAM_OWNER', 'PROJECT_OFFICE', 'ADMIN')")
    public Mono<MarketDTO> getMarket(@PathVariable String marketId){
        return marketService.getMarket(marketId);
    }


    @PostMapping("/create")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE', 'ADMIN')")
    public Mono<MarketDTO> createMarket(@RequestBody MarketDTO market){
        return marketService.createMarket(market)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось создать биржу")));
    }

    @DeleteMapping("/delete/{marketId}")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE', 'ADMIN')")
    public Mono<InfoResponse> deleteMarket(@PathVariable String marketId){
        return marketService.deleteMarket(marketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить биржу"));
    }


    @PutMapping("/update/{marketId}")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE', 'ADMIN')")
    public Mono<MarketDTO> updateMarket(@PathVariable String marketId, @RequestBody MarketDTO market){
        return marketService.updateMarket(marketId, market)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при редактировании")));
    }

    @PutMapping("/status/{marketId}/{status}")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE', 'ADMIN')")
    public Mono<MarketDTO> updateStatus(@PathVariable String marketId,
                                        @PathVariable MarketStatus status,
                                        @AuthenticationPrincipal User user){
        return marketService.updateStatus(marketId, status, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка")));
    }
}
