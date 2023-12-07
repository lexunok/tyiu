package com.tyiu.corn.controller;


import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.*;
import com.tyiu.corn.model.entities.TeamInvitation;
import com.tyiu.corn.model.entities.TeamRequest;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.RequestStatus;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.TeamService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@RequestMapping("/api/v1/team")
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
    public Mono<TeamDTO> getTeam(@PathVariable String teamId){
        return teamService.getTeam(teamId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/all")
    public Flux<TeamDTO> getTeams() {
        return teamService.getTeams();
    }

    @GetMapping("/users/{teamId}")
    public Flux<TeamMemberDTO> getUsersInTeamWithSkills(@PathVariable String teamId) {
        return teamService.getUsersInTeamWithSkills(teamId);
    }

    @GetMapping("/invites")
    public Flux<TeamInvitation> getInvitation(@AuthenticationPrincipal User user) {
        return teamService.getInvitations(user.getId())
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }
    @GetMapping("/requests/{teamId}")
    public Flux<TeamRequest> getTeamRequests(@PathVariable String teamId) {
        return teamService.getTeamRequests(teamId);
    }

    @GetMapping("/invitations/{teamId}")
    public Flux<TeamInvitation> getInvitationByTeam(@PathVariable String teamId) {
        return teamService.getInvitationByTeam(teamId);
    }

    @GetMapping("/users")
    public Flux<TeamMemberDTO> getAllUsersWithSkills(){
        return teamService.getAllUsersWithSkills();
    }

//    @GetMapping("/skills/{teamId}")
//    public Mono<TeamDTO> getTeamSkills(@PathVariable String teamId) {
//        return teamService.getTeamSkills(teamId);
//    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/add")
    @PreAuthorize("hasAuthority('TEAM_OWNER')")
    public Mono<TeamDTO> addTeam(@RequestBody TeamDTO team) {
        return teamService.addTeam(team)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/skill-filter/{role}")
    public Flux<TeamDTO> getTeamsBySkills(@RequestBody List<SkillDTO> checkedSkills, @PathVariable Role role){
        return teamService.getTeamsBySkills(checkedSkills, role);
    }

    @PostMapping("/vacancy-filter")
    public Flux<TeamDTO> getTeamsBySkills(@RequestBody List<SkillDTO> checkedSkills){
        return teamService.getTeamsByVacancies(checkedSkills);
    }

    @PostMapping("/request/send/{teamId}")
    public Mono<TeamRequest> sendTeamRequest(@PathVariable String teamId, @AuthenticationPrincipal User user) {
        return teamService.sendTeamRequest(teamId, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    /*@PostMapping("/send-invite/{teamId}")
    public Mono<Void> sendInvite(@PathVariable String teamId, @PathVariable List<UserDTO> users, User userInviter){
        return teamService.sendInviteToUser(teamId, users, userInviter)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }*/
    @PostMapping("/send-invites/{teamId}")
    public Flux<TeamInvitation> sendInvites(@PathVariable String teamId, @RequestBody List<UserDTO> users, @AuthenticationPrincipal User user){
        return teamService.sendInvitesToUsers(teamId, users, user);
    }

    @PostMapping("/invite/{teamId}/{userId}")
    public Mono<TeamMemberDTO> inviteInTeam(@PathVariable String teamId, @PathVariable String userId) {
        return teamService.addTeamMember(teamId, userId);
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{teamId}")
    @PreAuthorize("hasAuthority('TEAM_OWNER') || hasAuthority('ADMIN')")
    public Mono<InfoResponse> deleteTeam(@PathVariable String teamId, @AuthenticationPrincipal User user) {
        return teamService.deleteTeam(teamId, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @DeleteMapping("/delete/invite/{inviteId}")
    public Mono<InfoResponse> deleteInvite(@PathVariable String inviteId) {
        return teamService.deleteInvite(inviteId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @DeleteMapping("/delete/request/{requestId}")
    public Mono<Void> deleteRequest(@PathVariable String requestId) {
        return teamService.deleteRequest(requestId);
    }

    @DeleteMapping("/kick/{teamId}/{userId}")
    public Mono<Void> kickFromTeam(@PathVariable String teamId, @PathVariable String userId) {
        return teamService.kickFromTeam(teamId, userId);
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/update/{teamId}")
    @PreAuthorize("hasAuthority('TEAM_OWNER')")
    public Mono<TeamDTO> updateTeam(@PathVariable String teamId, @RequestBody TeamDTO team) {
        return teamService.updateTeam(teamId, team);
    }

    @PutMapping("/skills/update/{teamId}")
    public Mono<Void> updateTeamSkills(@PathVariable String teamId, @RequestBody Flux<SkillDTO> wantedSkills) {
        return teamService.updateTeamSkills(teamId, wantedSkills);
    }

    @PutMapping("/request/{requestId}/update/{newStatus}")
    public Mono<TeamRequest> updateTeamRequestStatus(@PathVariable String requestId, @PathVariable RequestStatus newStatus) {
        return teamService.updateTeamRequestStatus(requestId, newStatus)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PutMapping("/invitation/{invitationId}/update/{newStatus}")
    public Mono<TeamInvitation> updateTeamInvitationStatus(@PathVariable String invitationId, @PathVariable RequestStatus newStatus) {
        return teamService.updateTeamInvitationStatus(invitationId, newStatus)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PutMapping("/change/leader/{teamId}/{userId}")
    @PreAuthorize("hasAuthority('TEAM_OWNER')")
    public Mono<Void> changeLeader(@PathVariable String teamId, @PathVariable String userId){
        return teamService.changeTeamLeader(teamId, userId);
    }
}
