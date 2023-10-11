package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.ProjectDTO;
import com.tyiu.corn.model.dto.SkillDTO;
import com.tyiu.corn.model.dto.TeamDTO;
import com.tyiu.corn.model.dto.TeamMemberDTO;
import com.tyiu.corn.model.entities.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
@CacheConfig(cacheNames = "projects")
public class ProjectService {

    private final ReactiveMongoTemplate mongoTemplate;
    private final ModelMapper mapper;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Cacheable
    public Flux<ProjectDTO> getAll(){
        return mongoTemplate.findAll(Project.class).flatMap(p -> {
            ProjectDTO projectDTO = ProjectDTO.builder()
                    .id(p.getId())
                    .name(p.getName())
                    .description(p.getDescription())
                    .membersCount(p.getMembersCount())
                    .build();
            return mongoTemplate.find(Query.query(Criteria.where("id").in(p.getSkills())), Skill.class)
                    .flatMap(s -> Flux.just(mapper.map(s, SkillDTO.class)))
                    .collectList().flatMap(skills -> {
                        projectDTO.setSkills(skills);
                        return Mono.just(projectDTO);
                    });
        }).onErrorResume(ex -> Mono.error(new ErrorException("Failed to get a list of projects")));
    }

    @Cacheable
    public Mono<ProjectDTO> getProject(String id){
        return mongoTemplate.findById(id, Project.class).flatMap(p -> {
            ProjectDTO projectDTO = ProjectDTO.builder()
                    .id(p.getId())
                    .name(p.getName())
                    .description(p.getDescription())
                    .build();
            return mongoTemplate.find(Query.query(Criteria.where("id").in(p.getSkills())), Skill.class)
                    .flatMap(s -> Flux.just(mapper.map(s, SkillDTO.class)))
                    .collectList().flatMap(skills -> {
                        projectDTO.setSkills(skills);
                        return Mono.empty();
                    })
                    .then(mongoTemplate.findById(p.getTeamId(), Team.class).flatMap(t ->
                        mongoTemplate.find(Query.query(Criteria.where("email").in(t.getMembers())), User.class).flatMap(u ->
                            mongoTemplate.find(Query.query(Criteria.where("userEmail").is(u.getEmail())), UserSkill.class)
                                    .collectList().flatMap(s -> Mono.just(TeamMemberDTO.builder()
                                            .email(u.getEmail())
                                            .firstName(u.getFirstName())
                                            .lastName(u.getLastName())
                                            .skills(s)
                                            .build())))
                            .collectList().flatMap(users -> {
                                projectDTO.setTeam(mapper.map(t, TeamDTO.class));
                                projectDTO.getTeam().setMembers(users);
                                projectDTO.setMembersCount(t.getMembersCount());
                                return Mono.just(projectDTO);
                    })));
        }).onErrorResume(ex -> Mono.error(new ErrorException("Failed to get the project")));
    }

    @Cacheable
    public Flux<ProjectInvitation> getProjectInvitations(String email){
        return mongoTemplate.find(Query.query(Criteria.where("receiverEmail").is(email)), ProjectInvitation.class)
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to receive invitations")));
    }

    @Cacheable
    public Flux<ProjectRequest> getProjectApplications(String id){
        return mongoTemplate.find(Query.query(Criteria.where("projectId").is(id)), ProjectRequest.class)
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to receive invitations")));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<ProjectDTO> createProject(ProjectDTO projectDTO){
        Project project = Project.builder()
                .name(projectDTO.getName())
                .description(projectDTO.getDescription())
                .teamId(projectDTO.getTeam().getId())
                .membersCount(projectDTO.getTeam().getMembers().size())
                .build();

        List<SkillDTO> skillsDTO = projectDTO.getSkills();
        if (skillsDTO != null)
        {
            List<String> skills = new ArrayList<>();
            skillsDTO.forEach(s -> skills.add(s.getId()));
            project.setSkills(skills);
        }

        return mongoTemplate.save(project).flatMap(p -> {
            projectDTO.setId(p.getId());
            projectDTO.setMembersCount(p.getMembersCount());
            return Mono.just(projectDTO);
        })
        .onErrorResume(ex -> Mono.error(new ErrorException("Failed to create a project")));
    }

    @CacheEvict(allEntries = true)
    public Mono<ProjectInvitation> sendInvitation(String email, String projectId){
        return mongoTemplate.findById(projectId, Project.class).flatMap(t ->
                mongoTemplate.save(ProjectInvitation.builder()
                        .receiverEmail(email)
                        .projectId(projectId)
                        .projectName(t.getName())
                        .createdAt(Instant.now())
                        .build()))
        .onErrorResume(ex -> Mono.error(new ErrorException("Failed to send invitation")));
    }

    @CacheEvict(allEntries = true)
    public Mono<ProjectRequest> sendApplication(String email, String projectId){
        return mongoTemplate.findOne(Query.query(Criteria.where("email").is(email)), User.class)
                .flatMap(u -> mongoTemplate.find(Query.query(Criteria.where("userEmail").is(email)), UserSkill.class).collectList()
                        .flatMap(s -> mongoTemplate.save(ProjectRequest.builder()
                            .projectId(projectId)
                            .sender(TeamMemberDTO.builder()
                                    .email(u.getEmail())
                                    .firstName(u.getFirstName())
                                    .lastName(u.getLastName())
                                    .skills(s)
                                    .build())
                            .createdAt(Instant.now())
                            .build())))
        .onErrorResume(ex -> Mono.error(new ErrorException("Failed to send application")));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteProject(String id){
        return mongoTemplate.remove(Query.query(Criteria.where("id").is(id)), Project.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to delete the project")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteInvite(String id){
        return mongoTemplate.remove(Query.query(Criteria.where("id").is(id)), ProjectInvitation.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to delete the invite")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteApplication(String id){
        return mongoTemplate.remove(Query.query(Criteria.where("id").is(id)), ProjectRequest.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to delete the application")));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> updateProject(ProjectDTO projectDTO, String id){
        return mongoTemplate.findById(id, Project.class).flatMap(p -> {

            List<TeamMemberDTO> teamMemberDTOS = projectDTO.getTeam().getMembers();
            List<String> eUsers = new ArrayList<>();
            if (teamMemberDTOS != null)
            {
                teamMemberDTOS.forEach(u -> eUsers.add(u.getEmail()));
            }

            List<SkillDTO> skillsDTO = projectDTO.getSkills();
            if (skillsDTO != null)
            {
                List<String> skills = new ArrayList<>();
                skillsDTO.forEach(s -> skills.add(s.getId()));
                p.setSkills(skills);
            }

            p.setName(projectDTO.getName());
            p.setDescription(projectDTO.getDescription());
            return mongoTemplate.findById(p.getTeamId(), Team.class).flatMap(t -> {
                t.setMembers(eUsers);
                p.setMembersCount(eUsers.size());
                return mongoTemplate.save(p).then();
            });
        }).onErrorResume(ex -> Mono.error(new ErrorException("Failed to update the project")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> addInProject(String projectId, String email){
        return mongoTemplate.findById(projectId, Project.class).flatMap(p ->
                mongoTemplate.findById(p.getTeamId(), Team.class).flatMap(t -> {
                    t.getMembers().add(email);
                    t.setMembersCount(t.getMembers().size());
                    return mongoTemplate.save(t)
                            .then(mongoTemplate.save(p))
                            .then(mongoTemplate.remove(Query.query(Criteria.where("projectId").is(projectId)
                                    .and("receiverEmail").is(email)),ProjectInvitation.class))
                            .then(mongoTemplate.remove(Query.query(Criteria.where("projectId").is(projectId)
                                    .and("sender.getEmail").is(email)), ProjectRequest.class));
                }))
                .then().onErrorResume(ex -> Mono.error(new ErrorException("Failed to add user to project")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> kickFromProject(String projectId, String email){
        return mongoTemplate.findById(projectId, Project.class).flatMap(p ->
                mongoTemplate.findById(p.getTeamId(), Team.class).flatMap(t -> {
                    t.getMembers().remove(email);
                    t.setMembersCount(t.getMembers().size());
                    return mongoTemplate.save(t).then(mongoTemplate.save(p));
                }))
                .then().onErrorResume(ex -> Mono.error(new ErrorException("User could not be excluded from the project")));
    }
}
