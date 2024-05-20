package com.tyiu.ideas.controller;

import com.tyiu.client.exceptions.NotFoundException;
import com.tyiu.client.models.Role;
import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.*;
import com.tyiu.ideas.model.dto.*;
import com.tyiu.ideas.model.enums.*;
import com.tyiu.ideas.model.entities.*;
import com.tyiu.ideas.model.requests.*;
import com.tyiu.ideas.service.TeamService;
import com.tyiu.ideas.model.responses.InfoResponse;

import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;
import org.springframework.http.HttpStatus;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;

import java.util.List;

@RestController
@RequestMapping("/api/v1/ideas-service/team")
@RequiredArgsConstructor
public class TeamController {

    private final TeamService teamService;


    @GetMapping("/{teamId}")
    public Mono<TeamDTO> getTeam(@PathVariable String teamId, @AuthenticationPrincipal User user) {
        return teamService.getTeam(teamId, user.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось загрузить команду")));
    }

    @GetMapping("/all")
    public Flux<TeamDTO> getTeams(@AuthenticationPrincipal User user) {
        return teamService.getTeams(user.getId());
    }

    @GetMapping("/owner/all/{ideaMarketId}")
    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Flux<TeamDTO> getOwnerTeams(@AuthenticationPrincipal User user, @PathVariable String ideaMarketId) {
        return teamService.getOwnerTeams(user.getId(), ideaMarketId);
    }

    @GetMapping("/users")
    public Flux<TeamMemberDTO> getAllUsersWithSkills(){
        return teamService.getAllUsersWithSkills();
    }

    @GetMapping("/invites")
    public Flux<TeamInvitation> getInvitations(@AuthenticationPrincipal User user) {
        return teamService.getInvitations(user.getId());
    }

    @GetMapping("/users/requests/{teamId}")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Flux<TeamRequest> getTeamRequests(@PathVariable String teamId) {
        return teamService.getTeamRequests(teamId);
    }

    @GetMapping("/invitations/{teamId}")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Flux<TeamInvitation> getInvitationByTeam(@PathVariable String teamId) {
        return teamService.getInvitationByTeam(teamId);
    }

    @GetMapping("/idea/requests/{teamId}")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('TEACHER') || hasAuthority('INITIATOR') || hasAuthority('PROJECT_OFFICE') ||  hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Flux<TeamMarketRequestDTO> getTeamMarketRequests(@PathVariable String teamId){
        return teamService.getTeamMarketRequests(teamId);
    }

    @GetMapping("/users/consist")
    public Flux<TeamMemberDTO> getAllUsersInTeams(){
        return teamService.getAllUsersInTeams();
    }

    @GetMapping("/projects/{teamId}")
    @PreAuthorize("hasAnyAuthority('MEMBER','TEACHER','INITIATOR','PROJECT_OFFICE','TEAM_OWNER','ADMIN')")
    public Flux<ProjectDTO> getAllProjectsForTeam(@PathVariable String teamId){
        return teamService.getAllProjectsForTeam(teamId);
    }


    @PostMapping("/add")
    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Mono<TeamDTO> addTeam(@RequestBody TeamDTO team) {
        return teamService.addTeam(team)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при создании команды")));
    }

    @PostMapping("/skill-filter/{role}")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('INITIATOR') || hasAuthority('PROJECT_OFFICE') || hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Flux<TeamDTO> getTeamsBySkills(@RequestBody List<SkillDTO> checkedSkills, @PathVariable Role role, @AuthenticationPrincipal User user) {
        return teamService.getTeamsBySkills(checkedSkills, role, user.getId());
    }

    @PostMapping("/vacancy-filter")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('INITIATOR') || hasAuthority('PROJECT_OFFICE') || hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Flux<TeamDTO> getTeamsByVacancies(@RequestBody List<SkillDTO> checkedSkills, @AuthenticationPrincipal User user) {
        return teamService.getTeamsByVacancies(checkedSkills, user.getId());
    }

    @PostMapping("/request/send/{teamId}")
    @PreAuthorize("hasAuthority('MEMBER') || hasAuthority('ADMIN')")
    public Mono<TeamRequest> sendTeamRequest(@PathVariable String teamId, @AuthenticationPrincipal User user) {
        return teamService.sendTeamRequest(teamId, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при подачи заявки")));
    }
//TODO
//    @PostMapping("/send-invites")
//    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
//    public Flux<TeamInvitation> sendInvites(@RequestBody Flux<TeamInvitation> users, @AuthenticationPrincipal User user) {
//        return teamService.sendInvitesToUsers(users, user);
//    }

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

    @PostMapping("/get-team-members")
    public Flux<TeamWithMembersDTO> getTeamMembers(@RequestBody Flux<String> teamIds) {
        return teamService.getTeamMembers(teamIds);
    }

    @PostMapping("/get-full-names")
    public Flux<UsersFullNamesDTO> getUsersFullNames(@RequestBody Flux<UsersFromAndToByResultIdRequest> requests) {
        return teamService.getUsersFullNames(requests);
    }


    @DeleteMapping("/delete/{teamId}")
    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteTeam(@PathVariable String teamId, @AuthenticationPrincipal User user) {
        return teamService.deleteTeam(teamId, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное удаление"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при удалении"));
    }

    @DeleteMapping("/kick/{teamId}/{userId}")
    public Mono<Void> kickFromTeam(@PathVariable String teamId, @PathVariable String userId) {
        return teamService.kickFromTeam(teamId, userId);
    }

    @DeleteMapping("/leave/{teamId}")
    public Mono<Void> leaveFromTeam(@PathVariable String teamId, @AuthenticationPrincipal User user) {
        return teamService.leaveFromTeam(teamId, user.getId());
    }


    @PutMapping("/market/{marketId}")
    @PreAuthorize("hasAnyAuthority('PROJECT_OFFICE', 'ADMIN')")
    public Mono<Void> setMarketForTeam(@PathVariable String marketId, @RequestBody Flux<TeamDTO> teams){
        return teamService.setMarketForTeam(teams, marketId);
    }

    @PutMapping("/update/{teamId}")
    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Mono<TeamDTO> updateTeam(@PathVariable String teamId, @RequestBody TeamDTO team, @AuthenticationPrincipal User user) {
        return teamService.updateTeam(teamId, team, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка при обновлении команды")));
    }

    @PutMapping("/skills/update/{teamId}")
    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> updateTeamSkills(@PathVariable String teamId,
                                               @RequestBody Flux<SkillDTO> wantedSkills,
                                               @AuthenticationPrincipal User user) {
        return teamService.updateTeamSkills(teamId, wantedSkills, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Компетенции успешно изменены"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось изменить компетенции"));
    }

    @PutMapping("/request/{requestId}/update/{newStatus}")
    public Mono<TeamRequest> updateTeamRequestStatus(@PathVariable String requestId,
                                                     @PathVariable RequestStatus newStatus,
                                                     @AuthenticationPrincipal User user) {
        return teamService.updateTeamRequestStatus(requestId, newStatus, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка")));
    }

    @PutMapping("/invitation/{invitationId}/update/{newStatus}")
    public Mono<TeamInvitation> updateTeamInvitationStatus(@PathVariable String invitationId,
                                                           @PathVariable RequestStatus newStatus,
                                                           @AuthenticationPrincipal User user) {
        return teamService.updateTeamInvitationStatus(invitationId, newStatus, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибка")));
    }

    @PutMapping("/change/leader/{teamId}/{userId}")
    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> changeLeader(@PathVariable String teamId,
                                           @PathVariable String userId,
                                           @AuthenticationPrincipal User user) {
        return teamService.changeTeamLeader(teamId, userId, user)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Успешное изменение лидера"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Не удалось назначить лидера"));
    }
}
