package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.IdeaMarketDTO;
import com.tyiu.corn.model.dto.TeamMarketRequestDTO;
import com.tyiu.corn.service.IdeaMarketService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;
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
    public Flux<IdeaMarketDTO> getAllMarketIdeas(Principal principal) {
        return ideaMarketService.getAllMarketIdeas(Long.valueOf(principal.getName()));
    }

    @GetMapping("initiator/all")
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(Principal principal) {
        return ideaMarketService.getAllInitiatorMarketIdeas(Long.valueOf(principal.getName()));
    }

    @GetMapping("/{ideaMarketId}")
    public Mono<IdeaMarketDTO> getOneMarketIdea(Principal principal, @PathVariable Long ideaMarketId) {
        return ideaMarketService.getMarketIdea(Long.valueOf(principal.getName()), ideaMarketId);
    }

    @GetMapping("/favorite")
    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(Principal principal) {
        return ideaMarketService.getAllFavoriteMarketIdeas(Long.valueOf(principal.getName()));
    }

    @GetMapping("/requests/{ideaMarketId}")
    public Flux<TeamMarketRequestDTO> getAllTeamMarketIdeaRequests(@PathVariable Long ideaMarketId) {
        return ideaMarketService.getAllTeamsRequests(ideaMarketId);
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/send")
    public Flux<IdeaMarketDTO> createMarketIdea(@RequestBody List<IdeaDTO> ideaDTOList) {
        return ideaMarketService.sendIdeaOnMarket(ideaDTOList);
    }

    @PostMapping("/declare")
    public Mono<TeamMarketRequestDTO> createTeamMarketRequest(@RequestBody TeamMarketRequestDTO teamMarketRequestDTO) {
        return ideaMarketService.declareTeam(teamMarketRequestDTO);
    }

    @PostMapping("/favorite/{ideaMarketId}")
    public Mono<Void> makeMarketIdeaFavorite(Principal principal, @PathVariable Long ideaMarketId) {
        return ideaMarketService.makeMarketIdeaFavorite(Long.valueOf(principal.getName()), ideaMarketId);
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/idea/{ideaMarketId}")
    public Mono<Void> deleteMarketIdea(@PathVariable Long ideaMarketId) {
        return ideaMarketService.deleteMarketIdea(ideaMarketId);
    }

    @DeleteMapping("/delete/request/{teamMarketRequestId}")
    public Mono<Void> deleteTeamMarketRequest(@PathVariable Long teamMarketRequestId) {
        return ideaMarketService.deleteTeamMarketRequest(teamMarketRequestId);
    }

    @DeleteMapping("/unfavorite/{ideaMarketId}")
    public Mono<Void> deleteFavoriteMarketIdea(Principal principal, @PathVariable Long ideaMarketId) {
        return ideaMarketService.deleteMarketIdeaFromFavorite(Long.valueOf(principal.getName()), ideaMarketId);
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/accept/{teamMarketId}")
    public Mono<Void> acceptTeam(@PathVariable Long teamMarketId) {
        return ideaMarketService.acceptTeam(teamMarketId);
    }
}
