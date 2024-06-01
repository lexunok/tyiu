package com.tyiu.ideas.controller;

import com.nimbusds.jose.shaded.gson.Gson;
import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.dto.*;
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


    @GetMapping("/all")
    public Flux<IdeaMarketDTO> getAllMarketIdeas(@AuthenticationPrincipal Jwt user) {
        return ideaMarketService.getAllMarketIdeas(user.getId());
    }

    @GetMapping("/market/{marketId}/all")
    @PreAuthorize("hasAnyRole('MEMBER', 'TEACHER', 'PROJECT_OFFICE', 'TEAM_OWNER', 'ADMIN')")
    public Flux<IdeaMarketDTO> getAllMarketIdeasForMarket(@AuthenticationPrincipal Jwt user, @PathVariable String marketId) {
        return ideaMarketService.getAllMarketIdeasForMarket(user.getId(), marketId);
    }

    @GetMapping("/market/{marketId}/initiator")
    @PreAuthorize("hasAnyRole('INITIATOR', 'ADMIN')")
    public Flux<IdeaMarketDTO> getAllInitiatorMarketIdeas(@AuthenticationPrincipal Jwt user, @PathVariable String marketId) {
        return ideaMarketService.getAllInitiatorMarketIdeas(user.getId(), marketId);
    }

    @GetMapping("/{ideaMarketId}")
    public Mono<IdeaMarketDTO> getOneMarketIdea(@AuthenticationPrincipal Jwt user, @PathVariable String ideaMarketId) {
        return ideaMarketService.getMarketIdea(user.getId(), ideaMarketId)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось загрузить идею")));
    }

    @GetMapping("/favourite/{marketId}")
    public Flux<IdeaMarketDTO> getAllFavoriteMarketIdeas(@AuthenticationPrincipal Jwt user, @PathVariable String marketId) {
        return ideaMarketService.getAllFavoriteMarketIdeas(user.getId(), marketId);
    }

    @GetMapping("/requests/{ideaMarketId}")
    @PreAuthorize("hasAnyRole('INITIATOR', 'TEAM_OWNER', 'ADMIN')")
    public Flux<TeamMarketRequestDTO> getAllTeamMarketIdeaRequests(@PathVariable String ideaMarketId) {
        return ideaMarketService.getAllTeamsRequests(ideaMarketId);
    }

    @GetMapping("/get/advertisements/{ideaMarketId}")
    public Flux<IdeaMarketAdvertisementDTO> getIdeaMarketAdvertisement(@PathVariable String ideaMarketId) {
        return ideaMarketService.getIdeaMarketAdvertisement(ideaMarketId);
    }

    @GetMapping("/access/check/{marketId}")
    public Mono<Boolean> checkOwnerAccessInMarket(@PathVariable String marketId, @AuthenticationPrincipal Jwt user){
        return ideaMarketService.checkOwnerAccessInMarket(marketId, user.getId());
    }


    @PostMapping("/send/{marketId}")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE', 'ADMIN')")
    public Flux<IdeaMarketDTO> createMarketIdea(@PathVariable String marketId, @RequestBody Flux<IdeaDTO> ideaDTOList) {
        return ideaMarketService.sendIdeaOnMarket(marketId, ideaDTOList)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось отправить идею на биржу")));
    }

    @PostMapping("/declare")
    @PreAuthorize("hasAnyRole('TEAM_OWNER', 'ADMIN')")
    public Mono<TeamMarketRequestDTO> createTeamMarketRequest(@RequestBody TeamMarketRequestDTO teamMarketRequestDTO,
                                                              @AuthenticationPrincipal Jwt user) {
        return ideaMarketService.declareTeam(teamMarketRequestDTO, user.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось заявить команду")));
    }

    @PostMapping("/add/advertisement")
    @PreAuthorize("hasAnyRole('INITIATOR', 'ADMIN')")
    public Mono<IdeaMarketAdvertisementDTO> addAdvertisement(@RequestBody IdeaMarketAdvertisementDTO ideaMarketAdvertisementDTO,
                                                             @AuthenticationPrincipal Jwt jwt) {
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        return ideaMarketService.addAdvertisement(ideaMarketAdvertisementDTO, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось создать объявление")));
    }


    @DeleteMapping("/delete/idea/{ideaMarketId}")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE', 'ADMIN')")
    public Mono<InfoResponse> deleteMarketIdea(@PathVariable String ideaMarketId, @AuthenticationPrincipal Jwt jwt) {
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        return ideaMarketService.deleteMarketIdea(ideaMarketId, user)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить идею"));
    }

    @DeleteMapping("/unfavorite/{ideaMarketId}")
    public Mono<InfoResponse> deleteFavoriteMarketIdea(@AuthenticationPrincipal Jwt user, @PathVariable String ideaMarketId) {
        return ideaMarketService.deleteMarketIdeaFromFavorite(user.getId(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея убрана из избранных"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось убрать идею из избранных"));
    }

    @DeleteMapping("/delete/advertisement/{ideaMarketAdvertisementId}")
    @PreAuthorize("hasAnyRole('INITIATOR', 'ADMIN')")
    public Mono<InfoResponse> deleteIdeaMarketAdvertisement(@PathVariable String ideaMarketAdvertisementId, @AuthenticationPrincipal Jwt jwt) {
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        return ideaMarketService.deleteIdeaMarketAdvertisement(ideaMarketAdvertisementId, user)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось удалить заявку"));
    }


    @PutMapping("/favorite/{ideaMarketId}")
    public Mono<InfoResponse> makeMarketIdeaFavorite(@AuthenticationPrincipal Jwt user, @PathVariable String ideaMarketId) {
        return ideaMarketService.makeMarketIdeaFavorite(user.getId(), ideaMarketId)
                .thenReturn(new InfoResponse(HttpStatus.OK, "Идея добавлена в избранные"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось добавить идею в избранные"));
    }

    @PutMapping("/idea-status/{ideaMarketId}/{status}")
    @PreAuthorize("hasAnyRole('PROJECT_OFFICE','ADMIN')")
    public Mono<Void> changeIdeaMarketStatus(@PathVariable String ideaMarketId, @PathVariable IdeaMarketStatusType status) {
        return ideaMarketService.changeIdeaMarketStatus(ideaMarketId, status);
    }

    @PutMapping("/change-status/request/{teamMarketId}/{status}")
    @PreAuthorize("hasAnyRole('INITIATOR', 'TEAM_OWNER', 'ADMIN')")
    public Mono<Void> changeRequestStatus(@PathVariable String teamMarketId, @PathVariable RequestStatus status,
                                          @AuthenticationPrincipal Jwt jwt) {
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        return ideaMarketService.changeRequestStatus(teamMarketId, status, user);
    }

    @PutMapping("/accept/request/{ideaMarketId}/{teamId}")
    @PreAuthorize("hasAnyRole('INITIATOR', 'ADMIN')")
    public Mono<TeamDTO> setAcceptedTeam(@PathVariable String ideaMarketId, @PathVariable String teamId,
                                         @AuthenticationPrincipal Jwt jwt) {
        Gson gson = new Gson();
        UserDTO user = gson.fromJson(jwt.getClaim("user").toString(), UserDTO.class);
        return ideaMarketService.setAcceptedTeam(ideaMarketId, teamId, user);
    }

    @PutMapping("/check/advertisement/{ideaMarketAdvertisementId}")
    public Mono<Void> updateCheckByAdvertisement(@PathVariable String ideaMarketAdvertisementId,
                                                                          @AuthenticationPrincipal Jwt jwt) {
        return ideaMarketService.updateCheckByAdvertisement(ideaMarketAdvertisementId, jwt.getSubject());
    }
}
