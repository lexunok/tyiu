package com.tyiu.corn.controller;


import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.TeamAccessionDTO;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.dto.TeamMemberDTO;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.InfoResponse;
import com.tyiu.corn.service.TeamService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
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
    public Mono<TeamDTO> getTeam(@PathVariable Long teamId){
        return teamService.getTeam(teamId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/all")
    public Flux<TeamDTO> getTeams() {
        return teamService.getTeams();
    }

    @GetMapping("/invites/{teamId}")
    public Flux<TeamAccessionDTO> getAccessions(@PathVariable Long teamId) {
        return teamService.getAccessions(teamId);
    }
    @GetMapping("/invite/{targetEmail}")
    public Mono<TeamAccessionDTO> getAccessionByTargetId(@PathVariable Long targetId) {
        return teamService.getAccessionByTargetId(targetId);
    }
    @GetMapping("/profile/{userId}")
    public Mono<TeamDTO> getTeamProfile(@PathVariable Long userId) {
        return teamService.getTeamProfile(userId);
    }
    @GetMapping("/profile/all")
    public Flux<TeamMemberDTO> getTeamProfiles() {
        return teamService.getTeamProfiles();
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


    @PostMapping("/respondToRequest/{teamId}/{userId}")
    public Mono<Void> responseToRequest(@PathVariable Long teamId, @PathVariable Long userId) {
        return teamService.responseToRequest(teamId, userId);
    }
    @PostMapping("/invite/users")
    public Mono<Void> inviteRegisteredUsers(@RequestBody List<User> users) {
        return teamService.inviteRegisteredUsers(users);
    }

    @PostMapping("/invite/emaıl")
    public Mono<Void> inviteUnregisteredUser(@RequestParam String email, @RequestParam Long teamId) {
        return teamService.inviteUnregisteredUser(email, teamId);
    }
    @PostMapping
    public Mono<Void> sendRequest(@RequestBody TeamAccessionDTO teamAccessionDTO) {
        return teamService.sendRequest(teamAccessionDTO);
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
    @DeleteMapping("/delete/accession/{accessionId}")
    public Mono<Void> deleteAccession(@PathVariable Long accessionId) {
        return teamService.deleteAccession(accessionId);
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
    @PutMapping("/{accessionId}")
    public Mono<Void> responseToInvitation(@PathVariable Long accessionId, @RequestBody TeamAccessionDTO teamAccessionDTO) {
        return teamService.responseToInvitation(accessionId, teamAccessionDTO);
    }

}
