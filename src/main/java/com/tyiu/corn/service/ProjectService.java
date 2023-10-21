package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.ProjectDTO;
import com.tyiu.corn.model.entities.*;
import com.tyiu.corn.model.entities.mappers.ProjectMapper;
import com.tyiu.corn.model.entities.relations.Team2Member;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

import static org.springframework.data.relational.core.query.Update.update;
import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
@Slf4j
@CacheConfig(cacheNames = "projects")
public class ProjectService {

    private final R2dbcEntityTemplate template;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Cacheable
    public Flux<ProjectDTO> getAll(){
        return template.select(Project.class).all()
                .flatMap(p -> template.select(query(where("team_id").is(p.getTeamId())), Team2Member.class)
                        .collectList()
                        .flatMap(list -> Mono.just(ProjectDTO.builder()
                                .id(p.getId())
                                .name(p.getName())
                                .description(p.getDescription())
                                .membersCount(list.size())
                                .build())));
    }

    @Cacheable
    public Mono<ProjectDTO> getProject(Long projectId){
        String QUERY = "SELECT project.*, " +
                "t.id t_id, t.name t_name, t.description t_description, t.closed, t.created_at, t.owner_id, t.leader_id, " +
                "o.id o_id, o.email o_email, o.first_name o_first_name, o.last_name o_last_name, " +
                "l.id l_id, l.email l_email, l.first_name l_first_name, l.last_name l_last_name, " +
                "m.id m_id, m.email m_email, m.first_name m_first_name, m.last_name m_last_name, " +
                "s.id s_id, s.name s_name, s.type " +
                "FROM project " +
                "LEFT JOIN team t ON project.team_id = t.id " +
                "LEFT JOIN team_member ON t.id = team_member.team_id " +
                "LEFT JOIN team_skill ON t.id = team_skill.team_id " +
                "LEFT JOIN users o ON t.owner_id = o.id " +
                "LEFT JOIN users l ON t.leader_id = l.id " +
                "LEFT JOIN users m ON team_member.member_id = m.id " +
                "LEFT JOIN skill s ON team_skill.skill_id = s.id " +
                "WHERE project.id = :projectId";
        ProjectMapper projectMapper = new ProjectMapper();
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("projectId", projectId)
                .map(projectMapper::apply)
                .all()
                .collectList()
                .map(groupDTOMap -> groupDTOMap.get(0));
    }

    @Cacheable(cacheNames = "project_invitation")
    public Flux<ProjectInvitation> getProjectInvitations(Long id){
        return template.select(query(where("receiver_id").is(id)), ProjectInvitation.class);
    }

    @Cacheable(cacheNames = "project_request")
    public Flux<ProjectRequest> getProjectRequests(Long id){
        return template.select(query(where("project_id").is(id)), ProjectRequest.class);
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
                .build();

        return template.insert(project).flatMap(p -> {
            projectDTO.setId(p.getId());
            projectDTO.setMembersCount(projectDTO.getTeam().getMembers().size());

            return Mono.just(projectDTO);
        });
    }

    @CacheEvict(allEntries = true)
    public Mono<ProjectInvitation> sendInvitation(Long userId, Long projectId){
        return template.selectOne(query(where("id").is(projectId)), Project.class).flatMap(p ->
                template.insert(ProjectInvitation.builder()
                        .receiverId(userId)
                        .projectId(projectId)
                        .projectName(p.getName())
                        .createdAt(LocalDate.now())
                        .build()));
    }

    @CacheEvict(allEntries = true)
    public Mono<ProjectRequest> sendApplication(Long userId, Long projectId){
        return template.selectOne(query(where("id").is(userId)), User.class)
                .flatMap(u -> template.insert(ProjectRequest.builder()
                        .projectId(projectId)
                        .userId(u.getId())
                        .email(u.getEmail())
                        .firstName(u.getFirstName())
                        .lastName(u.getLastName())
                        .createdAt(LocalDate.now())
                        .build()));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteProject(Long id){
        return template.delete(query(where("id").is(id)), Project.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteInvite(Long id){
        return template.delete(query(where("id").is(id)), ProjectInvitation.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteRequest(Long id){
        return template.delete(query(where("id").is(id)), ProjectRequest.class).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> updateProject(ProjectDTO projectDTO, Long id){
        return template.update(query(where("id").is(id)),
                update("name", projectDTO.getName())
                        .set("description", projectDTO.getDescription()),
                Project.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> addInProject(Long projectId, Long userId){
        return template.selectOne(query(where("id").is(projectId)), Project.class)
                .flatMap(p -> template.insert(new Team2Member(p.getTeamId(), userId))
                        .then(template.delete(query(where("project_id").is(projectId)
                                .and("receiver_id").is(userId)),ProjectInvitation.class))
                        .then(template.delete(query(where("project_id").is(projectId)
                                .and("user_id").is(userId)), ProjectRequest.class)).then());
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> kickFromProject(Long projectId, Long userId){
        return template.selectOne(query(where("id").is(projectId)), Project.class)
                .flatMap(p -> template.delete(query(where("team_id").is(p.getTeamId())
                        .and("member_id").is(userId)),Team2Member.class)).then();
    }
}
