package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.enums.IdeaMarketStatusType;
import com.tyiu.ideas.model.enums.RequestStatus;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.IdeaMarketService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

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
    @PreAuthorize("hasRole('MEMBER') || hasRole('PROJECT_OFFICE') || hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Flux<IdeaMarketDTO> getAllMarketIdeasForMarket(@AuthenticationPrincipal Jwt jwt,
                                                          @PathVariable String marketId) {
        return ideaMarketService.getAllMarketIdeasForMarket(jwt.getId(), marketId);
    }

    @GetMapping("/market/{marketId}/initiator")
    @PreAuthorize("hasRole('INITIATOR') || hasRole('ADMIN')")
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(@AuthenticationPrincipal Jwt jwt,
                                                          @PathVariable String marketId) {
        return ideaMarketService.getAllInitiatorMarketIdeas(jwt.getId(), marketId);
    }

    @GetMapping("/{ideaMarketId}")
    public Mono<IdeaMarketDTO> getOneMarketIdea(@AuthenticationPrincipal Jwt jwt, @PathVariable String ideaMarketId) {
        return ideaMarketService.getMarketIdea(jwt.getId(), ideaMarketId)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось загрузить идею")));
    }

    @GetMapping("/favourite/{marketId}")
    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(@AuthenticationPrincipal Jwt jwt,
                                                         @PathVariable String marketId) {
        return ideaMarketService.getAllFavoriteMarketIdeas(jwt.getId(), marketId);
    }

    @GetMapping("/requests/{ideaMarketId}")
    @PreAuthorize("hasRole('INITIATOR') || hasRole('TEAM_OWNER') || hasRole('ADMIN')")
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
    @PreAuthorize("hasRole('PROJECT_OFFICE') || hasRole('ADMIN')")
    public Flux<IdeaMarketDTO> createMarketIdea(@PathVariable String marketId,
                                                @RequestBody Flux<IdeaDTO> ideaDTOList) {
        return ideaMarketService.sendIdeaOnMarket(marketId, ideaDTOList)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось отправить идею на биржу")));
    }

    @PostMapping("/declare")
    @PreAuthorize("hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Mono<TeamMarketRequestDTO> createTeamMarketRequest(@RequestBody TeamMarketRequestDTO teamMarketRequestDTO,
                                                              @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.declareTeam(teamMarketRequestDTO, jwt.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось заявить команду")));
    }

    @PostMapping("/add/advertisement")
    @PreAuthorize("hasRole('INITIATOR') || hasRole('ADMIN')")
    public Mono<IdeaMarketAdvertisementDTO> addAdvertisement(@RequestBody IdeaMarketAdvertisementDTO advertisementDTO,
                                                             @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.addAdvertisement(advertisementDTO, jwt)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось создать объявление")));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/idea/{ideaMarketId}")
    @PreAuthorize("hasRole('PROJECT_OFFICE') || hasRole('ADMIN')")
    public Mono<InfoResponse> deleteMarketIdea(@PathVariable String ideaMarketId, @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.deleteMarketIdea(ideaMarketId, jwt)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить идею"));
    }

    @DeleteMapping("/unfavorite/{ideaMarketId}")
    public Mono<InfoResponse> deleteFavoriteMarketIdea(@AuthenticationPrincipal Jwt jwt,
                                                       @PathVariable String ideaMarketId) {
        return ideaMarketService.deleteMarketIdeaFromFavorite(jwt.getId(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея убрана из избранных"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось убрать идею из избранных"));
    }

    @DeleteMapping("/delete/advertisement/{ideaMarketAdvertisementId}")
    @PreAuthorize("hasRole('INITIATOR') || hasRole('ADMIN')")
    public Mono<InfoResponse> deleteIdeaMarketAdvertisement(@PathVariable String ideaMarketAdvertisementId,
                                                            @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.deleteIdeaMarketAdvertisement(ideaMarketAdvertisementId, jwt)
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
    public Mono<InfoResponse> makeMarketIdeaFavorite(@AuthenticationPrincipal Jwt jwt,
                                                     @PathVariable String ideaMarketId) {
        return ideaMarketService.makeMarketIdeaFavorite(jwt.getId(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея добавлена в избранные"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось добавить идею в избранные"));
    }

    @PutMapping("/idea-status/{ideaMarketId}/{status}")
    @PreAuthorize("hasRole('INITIATOR') || hasRole('ADMIN')")
    public Mono<Void> changeIdeaMarketStatus(@PathVariable String ideaMarketId,
                                             @PathVariable IdeaMarketStatusType status,
                                             @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.changeIdeaMarketStatus(ideaMarketId, status, jwt);
    }

    @PutMapping("/change-status/request/{teamMarketId}/{status}")
    @PreAuthorize("hasRole('INITIATOR') || hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Mono<Void> changeRequestStatus(@PathVariable String teamMarketId,
                                          @PathVariable RequestStatus status,
                                          @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.changeRequestStatus(teamMarketId, status, jwt);
    }

    @PutMapping("/accept/request/{ideaMarketId}/{teamId}")
    @PreAuthorize("hasRole('INITIATOR') || hasRole('ADMIN')")
    public Mono<TeamDTO> setAcceptedTeam(@PathVariable String ideaMarketId,
                                         @PathVariable String teamId,
                                         @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.setAcceptedTeam(ideaMarketId, teamId, jwt);
    }

    @PutMapping("/check/advertisement/{ideaMarketAdvertisementId}")
    public Mono<Void> updateCheckByAdvertisement(@PathVariable String ideaMarketAdvertisementId,
                                                 @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.updateCheckByAdvertisement(ideaMarketAdvertisementId, jwt.getClaimAsString("sub"));
    }
}
