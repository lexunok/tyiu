package com.tyiu.ideas.controller;


import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.entities.TeamInvitation;
import com.tyiu.ideas.model.entities.TeamRequest;
import com.tyiu.ideas.model.enums.RequestStatus;
import com.tyiu.ideas.model.enums.Role;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.TeamService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@RequestMapping("/api/v1/ideas-service/team")
@RequiredArgsConstructor
public class TeamController {

    private final TeamService teamService;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @GetMapping("/{teamId}")
    public Mono<TeamDTO> getTeam(@PathVariable String teamId, @AuthenticationPrincipal Jwt jwt) {
        return teamService.getTeam(teamId, jwt.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось загрузить команду")));
    }

    @GetMapping("/all")
    public Flux<TeamDTO> getTeams(@AuthenticationPrincipal Jwt jwt) {
        return teamService.getTeams(jwt.getId());
    }

    @GetMapping("/owner/all/{ideaMarketId}")
    @PreAuthorize("hasAnyRole('TEAM_OWNER', 'ADMIN')")
    public Flux<TeamDTO> getOwnerTeams(@AuthenticationPrincipal Jwt jwt, @PathVariable String ideaMarketId) {
        return teamService.getOwnerTeams(jwt.getId(), ideaMarketId);
    }

    @GetMapping("/users")
    public Flux<TeamMemberDTO> getAllUsersWithSkills(){
        return teamService.getAllUsersWithSkills();
    }

    @GetMapping("/invites")
    public Flux<TeamInvitation> getInvitations(@AuthenticationPrincipal Jwt jwt) {
        return teamService.getInvitations(jwt.getId());
    }

    @GetMapping("/users/requests/{teamId}")
    @PreAuthorize("hasAnyRole('MEMBER', 'TEAM_OWNER', 'ADMIN')")
    public Flux<TeamRequest> getTeamRequests(@PathVariable String teamId) {
        return teamService.getTeamRequests(teamId);
    }

    @GetMapping("/invitations/{teamId}")
    @PreAuthorize("hasRole('MEMBER') || hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Flux<TeamInvitation> getInvitationByTeam(@PathVariable String teamId) {
        return teamService.getInvitationByTeam(teamId);
    }

    @GetMapping("/idea/requests/{teamId}")
    @PreAuthorize("hasRole('MEMBER') || hasRole('INITIATOR') || hasRole('PROJECT_OFFICE') ||  hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Flux<TeamMarketRequestDTO> getTeamMarketRequests(@PathVariable String teamId){
        return teamService.getTeamMarketRequests(teamId);
    }

    @GetMapping("/users/consist")
    public Flux<TeamMemberDTO> getAllUsersInTeams(){
        return teamService.getAllUsersInTeams();
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/add")
    @PreAuthorize("hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Mono<TeamDTO> addTeam(@RequestBody TeamDTO team) {
        return teamService.addTeam(team)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при создании команды")));
    }

    @PostMapping("/skill-filter/{role}")
    @PreAuthorize("hasRole('MEMBER') || hasRole('INITIATOR') || hasRole('PROJECT_OFFICE') || hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Flux<TeamDTO> getTeamsBySkills(@RequestBody List<SkillDTO> checkedSkills, @PathVariable Role role, @AuthenticationPrincipal Jwt jwt) {
        return teamService.getTeamsBySkills(checkedSkills, role, jwt.getId());
    }

    @PostMapping("/vacancy-filter")
    @PreAuthorize("hasRole('MEMBER') || hasRole('INITIATOR') || hasRole('PROJECT_OFFICE') || hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Flux<TeamDTO> getTeamsByVacancies(@RequestBody List<SkillDTO> checkedSkills, @AuthenticationPrincipal Jwt jwt) {
        return teamService.getTeamsByVacancies(checkedSkills, jwt.getId());
    }

    @PostMapping("/request/send/{teamId}")
    @PreAuthorize("hasRole('MEMBER') || hasRole('ADMIN')")
    public Mono<TeamRequest> sendTeamRequest(@PathVariable String teamId, @AuthenticationPrincipal Jwt jwt) {
        return teamService.sendTeamRequest(teamId, jwt)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при подачи заявки")));
    }

    @PostMapping("/send-invites")
    @PreAuthorize("hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Flux<TeamInvitation> sendInvites(@RequestBody Flux<TeamInvitation> users, @AuthenticationPrincipal Jwt jwt) {
        return teamService.sendInvitesToUsers(users, jwt);
    }

    @PostMapping("/invite/{teamId}/{userId}")
    public Mono<TeamMemberDTO> inviteInTeam(@PathVariable String teamId, @PathVariable String userId) {
        return teamService.addTeamMember(teamId, userId);
    }

    @PostMapping("/skills/users")
    public Flux<SkillDTO> getSkillsByUsers(@RequestBody List<UserDTO> users) {
        return teamService.getSkillsByUsers(users);
    }

    @PostMapping("/skills/invitations")
    public Flux<SkillDTO> getSkillsByInvitations(@RequestBody List<TeamInvitation> users) {
        return teamService.getSkillsByInvitations(users);
    }

    @PostMapping("/skills/requests")
    public Flux<SkillDTO> getSkillsByRequests(@RequestBody List<TeamRequest> users) {
        return teamService.getSkillsByRequests(users);
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{teamId}")
    @PreAuthorize("hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Mono<InfoResponse> deleteTeam(@PathVariable String teamId, @AuthenticationPrincipal Jwt jwt) {
        return teamService.deleteTeam(teamId, jwt)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при удалении"));
    }

    @DeleteMapping("/kick/{teamId}/{userId}")
    public Mono<Void> kickFromTeam(@PathVariable String teamId, @PathVariable String userId) {
        return teamService.kickFromTeam(teamId, userId);
    }

    @DeleteMapping("/leave/{teamId}")
    public Mono<Void> leaveFromTeam(@PathVariable String teamId, @AuthenticationPrincipal Jwt jwt) {
        return teamService.leaveFromTeam(teamId, jwt.getId());
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/update/{teamId}")
    @PreAuthorize("hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Mono<TeamDTO> updateTeam(@PathVariable String teamId, @RequestBody TeamDTO team, @AuthenticationPrincipal Jwt jwt) {
        return teamService.updateTeam(teamId, team, jwt)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при обновлении команды")));
    }

    @PutMapping("/skills/update/{teamId}")
    @PreAuthorize("hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Mono<InfoResponse> updateTeamSkills(@PathVariable String teamId,
                                               @RequestBody Flux<SkillDTO> wantedSkills,
                                               @AuthenticationPrincipal Jwt jwt) {
        return teamService.updateTeamSkills(teamId, wantedSkills, jwt)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Компетенции успешно изменены"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось изменить компетенции"));
    }

    @PutMapping("/request/{requestId}/update/{newStatus}")
    public Mono<TeamRequest> updateTeamRequestStatus(@PathVariable String requestId,
                                                     @PathVariable RequestStatus newStatus,
                                                     @AuthenticationPrincipal Jwt jwt) {
        return teamService.updateTeamRequestStatus(requestId, newStatus, jwt)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка")));
    }

    @PutMapping("/invitation/{invitationId}/update/{newStatus}")
    public Mono<TeamInvitation> updateTeamInvitationStatus(@PathVariable String invitationId,
                                                           @PathVariable RequestStatus newStatus,
                                                           @AuthenticationPrincipal Jwt jwt) {
        return teamService.updateTeamInvitationStatus(invitationId, newStatus, jwt)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка")));
    }

    @PutMapping("/change/leader/{teamId}/{userId}")
    @PreAuthorize("hasRole('TEAM_OWNER') || hasRole('ADMIN')")
    public Mono<InfoResponse> changeLeader(@PathVariable String teamId,
                                           @PathVariable String userId,
                                           @AuthenticationPrincipal Jwt jwt) {
        return teamService.changeTeamLeader(teamId, userId, jwt)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение лидера"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось назначить лидера"));
    }
}
