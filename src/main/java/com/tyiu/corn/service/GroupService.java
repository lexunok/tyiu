package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.User;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import com.tyiu.corn.model.entities.Group;
import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "groups")
public class GroupService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Cacheable
    public Flux<GroupDTO> getGroups() {
        return template.select(Group.class).all()
                .flatMap(g -> Flux.just(mapper.map(g, GroupDTO.class)))
                .switchIfEmpty(Mono.error(new ErrorException("Failed to get a list of groups")));
    }

    @Cacheable
    public Mono<GroupDTO> getGroupById(String id) {
        return template.selectOne(query(where("id").is(id)), Group.class)
                .flatMap(g -> {
                    GroupDTO groupDTO = mapper.map(g, GroupDTO.class);
                    return template.select(query(where("id").in(g.getUsersId())), User.class)
                            .flatMap(u -> Flux.just(mapper.map(u, UserDTO.class))).collectList()
                            .flatMap(list -> {
                                groupDTO.setUsers(list);
                                return Mono.just(groupDTO);
                            });
                }).switchIfEmpty(Mono.error(new ErrorException("Failed to get a group")));
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<GroupDTO> createGroup(GroupDTO groupDTO) {
        Group group = mapper.map(groupDTO, Group.class);
        List<Long> users = new ArrayList<>();
        groupDTO.getUsers().forEach(u -> users.add(u.getId()));
        group.setUsersId(users);
        return template.insert(group).flatMap(g -> {
            groupDTO.setId(g.getId());
            return Mono.just(groupDTO);
        }).switchIfEmpty(Mono.error(new ErrorException("Failed to create a group")));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteGroup(String id) {
        return template.delete(query(where("id").is(id)), Group.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to delete a group")));
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<GroupDTO> updateGroup(String id,GroupDTO groupDTO) {
        return template.selectOne(query(where("id").is(id)), Group.class)
                .flatMap(g -> {
                    g.setName(groupDTO.getName());
                    g.setUsersId(groupDTO.getUsers().stream().map(UserDTO::getId).toList());
                    g.setRoles(groupDTO.getRoles());
                    return template.insert(g).flatMap(group -> {
                        groupDTO.setId(g.getId());
                        return Mono.just(groupDTO);
                    });
                }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
}



