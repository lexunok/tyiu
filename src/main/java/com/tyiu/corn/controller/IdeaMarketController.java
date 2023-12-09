package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
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

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/market")
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

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/send")
    @PreAuthorize("hasAuthority('PROJECT_OFFICE') || hasAuthority('ADMIN')")
    public Flux<IdeaMarketDTO> createMarketIdea(@RequestBody List<IdeaMarketRequest> ideaDTOList) {
        return ideaMarketService.sendIdeaOnMarket(ideaDTOList)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось отправить идею на биржу")));
    }

    @PostMapping("/declare")
    public Mono<TeamMarketRequestDTO> createTeamMarketRequest(@RequestBody TeamMarketRequestDTO teamMarketRequestDTO) {
        return ideaMarketService.declareTeam(teamMarketRequestDTO)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось заявить команду")));
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

    @DeleteMapping("/delete/request/{teamMarketRequestId}")
    public Mono<InfoResponse> deleteTeamMarketRequest(@PathVariable String teamMarketRequestId) {
        return ideaMarketService.deleteTeamMarketRequest(teamMarketRequestId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить заявку"));
    }

    @DeleteMapping("/unfavorite/{ideaMarketId}")
    public Mono<InfoResponse> deleteFavoriteMarketIdea(@AuthenticationPrincipal User user, @PathVariable String ideaMarketId) {
        return ideaMarketService.deleteMarketIdeaFromFavorite(user.getId(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея убрана из избранных"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось убрать идею из избранных"));
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
    public Mono<TeamDTO> setAcceptedTeam(@PathVariable String ideaMarketId, @PathVariable String teamId) {
        return ideaMarketService.setAcceptedTeam(ideaMarketId, teamId);
    }

    @PutMapping("/reset/team/{ideaMarketId}")
    public Mono<Void> resetAcceptedTeam(@PathVariable String ideaMarketId) {
        return ideaMarketService.resetAcceptedTeam(ideaMarketId);
    }
}
