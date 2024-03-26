package com.tyiu.ideas.controller;

import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.enums.*;
import com.tyiu.ideas.model.entities.*;
import com.tyiu.ideas.model.responses.*;
import com.tyiu.ideas.config.exception.*;
import com.tyiu.ideas.service.IdeaMarketService;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;

import org.springframework.http.HttpStatus;

import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/ideas-service/market/idea")
public class IdeaMarketController {
    private final IdeaMarketService ideaMarketService;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @GetMapping("/all")
    public Flux<IdeaMarketDTO> getAllMarketIdeas(@AuthenticationPrincipal User user) {
        return ideaMarketService.getAllMarketIdeas(user.getId());
    }

    @GetMapping("/market/{marketId}/all")
    @PreAuthorize("hasAuthority('MEMBER')|| hasAuthority('TEACHER') || hasAuthority('PROJECT_OFFICE') || hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Flux<IdeaMarketDTO> getAllMarketIdeasForMarket(@PathVariable String marketId, @AuthenticationPrincipal User user) {
        return ideaMarketService.getAllMarketIdeasForMarket(marketId, user.getId());
    }

    @GetMapping("/market/{marketId}/initiator")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(@PathVariable String marketId, @AuthenticationPrincipal User user) {
        return ideaMarketService.getAllInitiatorMarketIdeas(marketId, user.getId());
    }

    @GetMapping("/{ideaMarketId}")
    public Mono<IdeaMarketDTO> getOneMarketIdea(@PathVariable String ideaMarketId,
                                                @AuthenticationPrincipal User user) {
        return ideaMarketService.getMarketIdea(ideaMarketId, user.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось загрузить идею")));
    }

    @GetMapping("/favourite/{marketId}")
    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(@PathVariable String marketId,
                                                         @AuthenticationPrincipal User user) {
        return ideaMarketService.getAllFavoriteMarketIdeas(marketId,user.getId());
    }

    @GetMapping("/requests/{ideaMarketId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Flux<TeamMarketRequestDTO> getAllTeamMarketIdeaRequests(@PathVariable String ideaMarketId) {
        return ideaMarketService.getAllTeamsRequests(ideaMarketId);
    }

    @GetMapping("/get/advertisements/{ideaMarketId}")
    public Flux<IdeaMarketAdvertisementDTO> getIdeaMarketAdvertisement(@PathVariable String ideaMarketId) {
        return ideaMarketService.getIdeaMarketAdvertisement(ideaMarketId);
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/send/{marketId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Flux<IdeaMarketDTO> createMarketIdea(@PathVariable String marketId,
                                                @RequestBody Flux<IdeaDTO> ideaDTOList,
                                                @AuthenticationPrincipal User user) {
        return ideaMarketService.sendIdeaOnMarket(marketId, ideaDTOList, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось отправить идею на биржу")));
    }

    @PostMapping("/declare")
    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Mono<TeamMarketRequestDTO> createTeamMarketRequest(@RequestBody TeamMarketRequestDTO teamMarketRequestDTO,
                                                              @AuthenticationPrincipal User user) {
        return ideaMarketService.declareTeam(teamMarketRequestDTO, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось заявить команду")));
    }

    @PostMapping("/add/advertisement")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<IdeaMarketAdvertisementDTO> addAdvertisement(@RequestBody IdeaMarketAdvertisementDTO ideaMarketAdvertisementDTO,
                                                             @AuthenticationPrincipal User user) {
        return ideaMarketService.addAdvertisement(ideaMarketAdvertisementDTO, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось создать объявление")));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/idea/{ideaMarketId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteMarketIdea(@PathVariable String ideaMarketId,
                                               @AuthenticationPrincipal User user) {
        return ideaMarketService.deleteMarketIdea(ideaMarketId, user)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить идею"));
    }

    @DeleteMapping("/unfavorite/{ideaMarketId}")
    public Mono<InfoResponse> deleteFavoriteMarketIdea(@PathVariable String ideaMarketId,
                                                       @AuthenticationPrincipal User user) {
        return ideaMarketService.deleteMarketIdeaFromFavorite(ideaMarketId, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея убрана из избранных"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось убрать идею из избранных"));
    }

    @DeleteMapping("/delete/advertisement/{ideaMarketAdvertisementId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteIdeaMarketAdvertisement(@PathVariable String ideaMarketAdvertisementId,
                                                            @AuthenticationPrincipal User user) {
        return ideaMarketService.deleteIdeaMarketAdvertisement(ideaMarketAdvertisementId, user)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить заявку"));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/favorite/{ideaMarketId}")
    public Mono<InfoResponse> makeMarketIdeaFavorite(@PathVariable String ideaMarketId,
                                                     @AuthenticationPrincipal User user) {
        return ideaMarketService.makeMarketIdeaFavorite(ideaMarketId, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея добавлена в избранные"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось добавить идею в избранные"));
    }

    @PutMapping("/idea-status/{ideaMarketId}/{status}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<Void> changeIdeaMarketStatus(@PathVariable String ideaMarketId, @PathVariable IdeaMarketStatusType status,
                                             @AuthenticationPrincipal User user) {
        return ideaMarketService.changeIdeaMarketStatus(ideaMarketId, status, user);
    }

    @PutMapping("/change-status/request/{teamMarketId}/{status}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Mono<Void> changeRequestStatus(@PathVariable String teamMarketId, @PathVariable RequestStatus status,
                                          @AuthenticationPrincipal User user) {
        return ideaMarketService.changeRequestStatus(teamMarketId, status, user);
    }

    @PutMapping("/accept/request/{ideaMarketId}/{teamId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<TeamDTO> setAcceptedTeam(@PathVariable String ideaMarketId, @PathVariable String teamId,
                                         @AuthenticationPrincipal User user) {
        return ideaMarketService.setAcceptedTeam(ideaMarketId, teamId, user);
    }

    @PutMapping("/check/advertisement/{ideaMarketAdvertisementId}")
    public Mono<Void> updateCheckByAdvertisement(@PathVariable String ideaMarketAdvertisementId,
                                                 @AuthenticationPrincipal User user) {
        return ideaMarketService.updateCheckByAdvertisement(ideaMarketAdvertisementId, user.getEmail());
    }
}
