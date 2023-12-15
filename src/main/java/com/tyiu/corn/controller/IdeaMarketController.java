package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.IdeaMarketAdvertisementDTO;
import com.tyiu.corn.model.dto.IdeaMarketDTO;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.dto.TeamMarketRequestDTO;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.IdeaMarketStatusType;
import com.tyiu.corn.model.enums.RequestStatus;
import com.tyiu.corn.model.requests.IdeaMarketRequest;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.IdeaMarketService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/market/idea")
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

    @GetMapping("/market/{marketId}")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Flux<IdeaMarketDTO> getAllMarketIdeasForMarket(@AuthenticationPrincipal User user, @PathVariable String marketId) {
        return ideaMarketService.getAllMarketIdeasForMarket(user.getId(), marketId);
    }

    @GetMapping("/initiator/all")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(@AuthenticationPrincipal User user) {
        return ideaMarketService.getAllInitiatorMarketIdeas(user.getId());
    }

    @GetMapping("/{ideaMarketId}")
    public Mono<IdeaMarketDTO> getOneMarketIdea(@AuthenticationPrincipal User user, @PathVariable String ideaMarketId) {
        return ideaMarketService.getMarketIdea(user.getId(), ideaMarketId)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось загрузить идею")));
    }

    @GetMapping("/favorite")
    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(@AuthenticationPrincipal User user) {
        return ideaMarketService.getAllFavoriteMarketIdeas(user.getId());
    }

    @GetMapping("/requests/{ideaMarketId}")
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
    public Flux<IdeaMarketDTO> createMarketIdea(@PathVariable String marketId, @RequestBody Flux<IdeaMarketRequest> ideaDTOList) {
        return ideaMarketService.sendIdeaOnMarket(marketId, ideaDTOList)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось отправить идею на биржу")));
    }

    @PostMapping("/declare")
    @PreAuthorize("hasAuthority('TEAM_MEMBER') || hasAuthority('ADMIN')")
    public Mono<TeamMarketRequestDTO> createTeamMarketRequest(@RequestBody TeamMarketRequestDTO teamMarketRequestDTO) {
        return ideaMarketService.declareTeam(teamMarketRequestDTO)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось заявить команду")));
    }

    @PostMapping("/add/advertisement")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<IdeaMarketAdvertisementDTO> addAdvertisement(@RequestBody IdeaMarketAdvertisementDTO ideaMarketAdvertisementDTO, @AuthenticationPrincipal User user) {
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
    public Mono<InfoResponse> deleteMarketIdea(@PathVariable String ideaMarketId) {
        return ideaMarketService.deleteMarketIdea(ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить идею"));
    }

    @DeleteMapping("/unfavorite/{ideaMarketId}")
    public Mono<InfoResponse> deleteFavoriteMarketIdea(@AuthenticationPrincipal User user, @PathVariable String ideaMarketId) {
        return ideaMarketService.deleteMarketIdeaFromFavorite(user.getId(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея убрана из избранных"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось убрать идею из избранных"));
    }

    @DeleteMapping("/delete/advertisement/{ideaMarketAdvertisementId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteIdeaMarketAdvertisement(@PathVariable String ideaMarketAdvertisementId) {
        return ideaMarketService.deleteIdeaMarketAdvertisement(ideaMarketAdvertisementId)
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
    public Mono<InfoResponse> makeMarketIdeaFavorite(@AuthenticationPrincipal User user, @PathVariable String ideaMarketId) {
        return ideaMarketService.makeMarketIdeaFavorite(user.getId(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея добавлена в избранные"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось добавить идею в избранные"));
    }

    @PutMapping("/idea-status/{ideaMarketId}/{status}")
    public Mono<Void> changeIdeaMarketStatus(@PathVariable String ideaMarketId, @PathVariable IdeaMarketStatusType status) {
        return ideaMarketService.changeIdeaMarketStatus(ideaMarketId, status);
    }

    @PutMapping("/change-status/request/{teamMarketId}/{status}")
    public Mono<Void> changeRequestStatus(@PathVariable String teamMarketId, @PathVariable RequestStatus status) {
        return ideaMarketService.changeRequestStatus(teamMarketId, status);
    }

    @PutMapping("/accept/request/{ideaMarketId}/{teamId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<TeamDTO> setAcceptedTeam(@PathVariable String ideaMarketId, @PathVariable String teamId) {
        return ideaMarketService.setAcceptedTeam(ideaMarketId, teamId);
    }

    @PutMapping("/check/advertisement/{ideaMarketAdvertisementId}")
    public Mono<Void> updateCheckByAdvertisement(@PathVariable String ideaMarketAdvertisementId,
                                                                          @AuthenticationPrincipal User user) {
        return ideaMarketService.updateCheckByAdvertisement(ideaMarketAdvertisementId, user.getEmail());
    }
}
