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

//    @PutMapping("/kick/{teamId}")
//    public Mono<Void> kickFromTeam(@PathVariable String teamId, @RequestBody AuthenticationResponse invitation){
//        return teamService.kickFromTeam(teamId, invitation.getEmail());
//    }
//
//    @PutMapping("/invite/{teamId}")
//    public Mono<Void> inviteInTeam(@PathVariable String teamId, @RequestBody AuthenticationResponse invitation){
//        return teamService.inviteInTeam(teamId, invitation.getEmail());
//    }
//
//    @PutMapping("/update/{teamId}")
//    public Mono<Void> updateTeam(@PathVariable String teamId,@RequestBody TeamDTO team){
//        return teamService.updateTeam(teamId, team);
//    }
}
