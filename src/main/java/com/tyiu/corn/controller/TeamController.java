package com.tyiu.corn.controller;


import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.entities.TeamInvitation;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.service.TeamService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/team")
@RequiredArgsConstructor
public class TeamController {

    private final TeamService teamService;

    @GetMapping("/{teamId}")
    public Mono<TeamDTO> getTeam(@PathVariable String teamId){
        return teamService.getTeam(teamId);
    }

    @GetMapping("/all")
    public Flux<TeamDTO> getTeams() {
        return teamService.getTeams();
    }

    @GetMapping("/invites/{receiverId}")
    public Flux<TeamInvitation> getInvitation(@PathVariable String receiverId) {
        return teamService.getInvitation(receiverId);
    }

    @PostMapping("/add")
    public Mono<TeamDTO> addTeam(@RequestBody TeamDTO team) {
        return teamService.addTeam(team);
    }

    @PostMapping("/send-invite/{teamId}")
    public Mono<TeamInvitation> sendInvite(@RequestBody AuthenticationResponse invitation, @PathVariable String teamId){
        return teamService.sendInviteToUser(invitation.getId(), teamId);
    }

    @DeleteMapping("/delete/{teamId}")
    public Mono<Void> deleteTeam(@PathVariable String teamId) {
        return teamService.deleteTeam(teamId);
    }

    @DeleteMapping("/delete/invite/{inviteId}")
    public Mono<Void> deleteInvite(@PathVariable String inviteId) {
        return teamService.deleteInvite(inviteId);
    }

    @PutMapping("/kick/{teamId}")
    public Mono<Void> kickFromTeam(@PathVariable String teamId, @RequestBody AuthenticationResponse invitation){
        return teamService.kickFromTeam(teamId, invitation.getId());
    }

    @PutMapping("/invite/{teamId}")
    public Mono<Void> inviteInTeam(@PathVariable String teamId, @RequestBody AuthenticationResponse invitation){
        return teamService.inviteInTeam(teamId, invitation.getId());
    }

    @PutMapping("/update/{teamId}")
    public Mono<Void> updateTeam(@PathVariable String teamId,@RequestBody TeamDTO team){
        return teamService.updateTeam(teamId, team);
    }
}
