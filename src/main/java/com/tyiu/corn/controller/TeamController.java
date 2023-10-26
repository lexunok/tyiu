package com.tyiu.corn.controller;


import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.entities.TeamInvitation;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.TeamService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.security.Principal;

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
    public Mono<TeamDTO> getTeam(@PathVariable Long teamId){
        return teamService.getTeam(teamId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/all")
    public Flux<TeamDTO> getTeams() {
        return teamService.getTeams()
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/invites")
    public Flux<TeamInvitation> getInvitation(Principal principal) {
        return teamService.getInvitations(Long.valueOf(principal.getName()))
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
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

    @PostMapping("/send-invite/{teamId}")
    public Mono<TeamInvitation> sendInvite(@RequestBody AuthenticationResponse invitation, @PathVariable Long teamId){
        return teamService.sendInviteToUser(invitation.getId(), teamId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @PostMapping("/invite/{teamId}")
    public Mono<InfoResponse> inviteInTeam(@PathVariable Long teamId, @RequestBody AuthenticationResponse invitation) {
        return teamService.inviteInTeam(teamId, invitation.getId())
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
    public Mono<InfoResponse> deleteTeam(@PathVariable Long teamId) {
        return teamService.deleteTeam(teamId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @DeleteMapping("/delete/invite/{inviteId}")
    public Mono<InfoResponse> deleteInvite(@PathVariable Long inviteId) {
        return teamService.deleteInvite(inviteId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success"));
    }

    @DeleteMapping("/delete/request/{requestId}")
    public Mono<Void> deleteRequest(@PathVariable Long requestId) {
        return teamService.deleteRequest(requestId);
    }

    @DeleteMapping("/kick/{teamId}")
    public Mono<Void> kickFromTeam(@PathVariable Long teamId, @RequestBody AuthenticationResponse invitation) {
        return teamService.kickFromTeam(teamId, invitation.getId());
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @PutMapping("/update/{teamId}")
    public Mono<Void> updateTeam(@PathVariable Long teamId, @RequestBody TeamDTO team) {
        return teamService.updateTeam(teamId, team);
    }
}
