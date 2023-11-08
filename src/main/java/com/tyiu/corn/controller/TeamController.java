package com.tyiu.corn.controller;


import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.TeamAccessionDTO;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.entities.TeamAccession;
import com.tyiu.corn.model.entities.TeamExitRequest;
import com.tyiu.corn.model.entities.TeamInvitation;
import com.tyiu.corn.model.entities.User;
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
    public Flux<TeamAccessionDTO> getInvitation() {
        return teamService.getInvitations()
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
    /////////////////
    //////////////////
    /////////////////
    /////////////////
    /////////////////
    ////////////////
    //////////////
    /////////////

    @GetMapping("/members/{teamId}")
    public Flux<User> getTeamMembers(@PathVariable Long teamId) {
        return teamService.getTeamMembers(teamId);
    }

    @PostMapping("/invite")
    public Mono<TeamAccession> sendInvite(@PathVariable  Long teamId, Long userId, String emails) {
        return teamService.sendInvite(teamId, userId, emails);
    }

    @PutMapping("/responseRequest/{accessionId}")
    public Mono<Void> responseRequest(@PathVariable Long accessionId, @RequestParam("accept") boolean accept) {
        return teamService.processAccession(accessionId, accept);
    }

    @DeleteMapping("/accession/delete/{accessionId}")
    public Mono<Void> deleteAccession(@PathVariable Long accessionId) {
        return teamService.deleteAccession(accessionId);
    }
    @PostMapping("/accept-invitation/{accessionId}")
    public Mono<Void> acceptInvitation(@PathVariable Long accessionId) {
        return teamService.acceptInvitation(accessionId);
    }

    @DeleteMapping("/reject/request/{requestId}")
    public Mono<Void> rejectTeamRequest(@PathVariable Long requestId) {
        return teamService.rejectTeamRequest(requestId);
    }

    @PostMapping("/exit")
    public Mono<Void> requestTeamExit(@RequestBody TeamExitRequest exitRequest) {
        return teamService.requestTeamExit(exitRequest.getTeamId(), exitRequest.getUserId());
    }
}
