package com.tyiu.corn.controller;


import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.dto.TeamMemberDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.TeamInvitation;
import com.tyiu.corn.model.entities.TeamRequest;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.RequestStatus;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.TeamService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
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
    @PutMapping("/request/{requestId}/update/{newStatus}")
    public Flux<TeamRequest> updateTeamRequestStatus(@PathVariable String requestId, @PathVariable RequestStatus newStatus) {
        return teamService.updateTeamRequestStatus(requestId, newStatus)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/request/send/{teamId}/{userId}")
    public Mono<TeamRequest> sendTeamRequest(@PathVariable String teamId, @PathVariable String userId, @RequestBody User user) {
        return teamService.sendTeamRequest(teamId, userId, user)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }
    @GetMapping("/skills/{teamId}")
    public Mono<TeamDTO> getTeamSkills(@PathVariable String teamId) {
        return teamService.getTeamSkills(teamId);
    }

    @PostMapping("/skills/update/{teamId}")
    public Mono<Void> updateTeamSkills(@PathVariable String teamId, @RequestBody List<SkillDTO> totalSkills, @RequestBody List<SkillDTO> wantedSkills) {
        return teamService.updateTeamSkills(teamId, totalSkills, wantedSkills);
    }

    @PostMapping("/skills/create/{teamId}")
    public Mono<Void> createTeamSkills(@PathVariable String teamId, @RequestBody List<SkillDTO> skillIds) {
        return teamService.createTeamSkills(teamId, skillIds);
    }

    @GetMapping("/users/{teamId}")
    public Flux<TeamMemberDTO> getUsersInTeamWithSkills(@PathVariable String teamId) {
        return teamService.getUsersInTeamWithSkills(teamId);
    }

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

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @PostMapping("/add")
    public Mono<TeamDTO> addTeam(@RequestBody TeamDTO team) {
        return teamService.addTeam(team)
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

    @PostMapping("/invite/{teamId}")
    public Mono<InfoResponse> inviteInTeam(@PathVariable String teamId, @AuthenticationPrincipal User user) {
        return teamService.inviteInTeam(teamId, user.getId())
                .thenReturn(new InfoResponse(HttpStatus.OK, "Success inviting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST, "Invite is not successful"));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @DeleteMapping("/delete/{teamId}")
    public Mono<InfoResponse> deleteTeam(@PathVariable String teamId) {
        return teamService.deleteTeam(teamId)
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

    @DeleteMapping("/kick/{teamId}")
    public Mono<Void> kickFromTeam(@PathVariable String teamId, @RequestBody AuthenticationResponse invitation) {
        return teamService.kickFromTeam(teamId, invitation.getId());
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/update/{teamId}")
    public Mono<TeamDTO> updateTeam(@PathVariable String teamId, @RequestBody TeamDTO team) {
        return teamService.updateTeam(teamId, team);
    }
}
