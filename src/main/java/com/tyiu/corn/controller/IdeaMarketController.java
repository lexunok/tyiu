package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.IdeaDTO;
import com.tyiu.corn.model.dto.IdeaMarketDTO;
import com.tyiu.corn.model.dto.TeamMarketRequestDTO;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.IdeaMarketService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
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
        return ideaMarketService.getAllMarketIdeas(principal.getName());
    }

    @GetMapping("/initiator/all")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(Principal principal) {
        return ideaMarketService.getAllInitiatorMarketIdeas(principal.getName());
    }

    @GetMapping("/{ideaMarketId}")
    public Mono<IdeaMarketDTO> getOneMarketIdea(Principal principal, @PathVariable String ideaMarketId) {
        return ideaMarketService.getMarketIdea(principal.getName(), ideaMarketId);
    }

    @GetMapping("/favorite")
    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(Principal principal) {
        return ideaMarketService.getAllFavoriteMarketIdeas(principal.getName());
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
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Flux<IdeaMarketDTO> createMarketIdea(@RequestBody List<IdeaDTO> ideaDTOList) {
        return ideaMarketService.sendIdeaOnMarket(ideaDTOList)
                .switchIfEmpty(Mono.error(new NotFoundException("Failed to send the idea to the market")));
    }

    @PostMapping("/declare")
    public Mono<TeamMarketRequestDTO> createTeamMarketRequest(@RequestBody TeamMarketRequestDTO teamMarketRequestDTO) {
        return ideaMarketService.declareTeam(teamMarketRequestDTO)
                .switchIfEmpty(Mono.error(new NotFoundException("Failed to declare a team in the idea")));
    }

    @PostMapping("/favorite/{ideaMarketId}")
    public Mono<InfoResponse> makeMarketIdeaFavorite(Principal principal, @PathVariable String ideaMarketId) {
        return ideaMarketService.makeMarketIdeaFavorite(principal.getName(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Idea added to favorites"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Failed to make the idea a favorite"));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/idea/{ideaMarketId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteMarketIdea(@PathVariable String ideaMarketId) {
        return ideaMarketService.deleteMarketIdea(ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Failed to remove the idea from the market"));
    }

    @DeleteMapping("/delete/request/{teamMarketRequestId}")
    public Mono<InfoResponse> deleteTeamMarketRequest(@PathVariable String teamMarketRequestId) {
        return ideaMarketService.deleteTeamMarketRequest(teamMarketRequestId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Couldn't delete the application from the idea"));
    }

    @DeleteMapping("/unfavorite/{ideaMarketId}")
    public Mono<InfoResponse> deleteFavoriteMarketIdea(Principal principal, @PathVariable String ideaMarketId) {
        return ideaMarketService.deleteMarketIdeaFromFavorite(principal.getName(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Idea removed from favorites"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Couldn't delete an idea from favorites"));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/accept/{teamMarketId}")
    @PreAuthorize("hasAuthority('INITIATOR') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> acceptTeam(@PathVariable String teamMarketId) {
        return ideaMarketService.acceptTeam(teamMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "The team is accepted into the idea"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Error when approving an idea"));
    }
}
