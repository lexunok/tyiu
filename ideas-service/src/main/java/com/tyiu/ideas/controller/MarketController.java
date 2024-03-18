package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.NotFoundException;
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

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @GetMapping("/all")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('TEACHER') || hasAuthority('ADMIN')")
    public Flux<MarketDTO> getAll(){
        return marketService.getAll();
    }

    @GetMapping("/active")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('TEACHER') || hasAuthority('INITIATOR') || hasAuthority('TEAM_OWNER') || hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Flux<MarketDTO> getActiveMarket(){
        return marketService.getActiveMarkets();
    }

    @GetMapping("/{marketId}")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('TEACHER') || hasAuthority('INITIATOR') || hasAuthority('TEAM_OWNER') || hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<MarketDTO> getMarket(@PathVariable String marketId){
        return marketService.getMarket(marketId);
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/create")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<MarketDTO> createMarket(@RequestBody MarketDTO market){
        return marketService.createMarket(market)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось создать биржу")));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{marketId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteMarket(@PathVariable String marketId){
        return marketService.deleteMarket(marketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить биржу"));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/update/{marketId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<MarketDTO> updateMarket(@PathVariable String marketId, @RequestBody MarketDTO market){
        return marketService.updateMarket(marketId, market)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при редактировании")));
    }

    @PutMapping("/status/{marketId}/{status}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<MarketDTO> updateStatus(@PathVariable String marketId,
                                        @PathVariable MarketStatus status,
                                        @AuthenticationPrincipal User user){
        return marketService.updateStatus(marketId, status, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка")));
    }
}
