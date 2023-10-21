package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.mappers.GroupMapper;
import com.tyiu.corn.model.entities.relations.Group2User;
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

import java.util.List;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "groups")
public class GroupService {

    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;
    private final GroupMapper groupMapper = new GroupMapper();

    private final String QUERY = "SELECT groups.*, users.id AS member_id, users.email, users.first_name, users.last_name " +
                                    "FROM groups " +
                                    "LEFT JOIN group_user ON groups.id = group_user.group_id " +
                                    "LEFT JOIN users ON group_user.user_id = users.id " +
                                    "WHERE groups.id = :groupId";

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Cacheable
    public Flux<GroupDTO> getGroups() {
        return template.select(Group.class).all()
                .flatMap(g -> Flux.just(mapper.map(g, GroupDTO.class)));
    }

    @Cacheable
    public Mono<GroupDTO> getGroupById(Long groupId) {
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("groupId", groupId)
                .map(groupMapper::apply)
                .all()
                .collectList()
                .map(groupDTOMap -> groupDTOMap.get(0));
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
        return template.insert(group).flatMap(g -> {
            groupDTO.setId(g.getId());
            groupDTO.getUsers().forEach(u -> template.insert(new Group2User(u.getId(), g.getId()))
                    .subscribe());
            return Mono.just(groupDTO);
        });
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteGroup(Long id) {
        return template.delete(query(where("id").is(id)), Group.class).then();
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<GroupDTO> updateGroup(Long groupId,GroupDTO groupDTO) {
        return template.getDatabaseClient()
                .sql(QUERY)
                .bind("groupId", groupId)
                .map(groupMapper::apply)
                .all()
                .collectList()
                .map(groupDTOMap -> groupDTOMap.get(0))
                .flatMap(g -> {
                    g.setName(groupDTO.getName());
                    g.setRoles(groupDTO.getRoles());

                    List<UserDTO> newUsers = groupDTO.getUsers();
                    List<UserDTO> oldUsers = g.getUsers();

                    if (!newUsers.equals(oldUsers))
                    {
                        oldUsers.forEach(u -> template.delete(query(where("group_id").is(groupId)
                                .and("user_id").is(u.getId())), Group2User.class).subscribe());
                        newUsers.forEach(u -> template.insert((new Group2User(u.getId(), groupId))).subscribe());
                        g.setUsers(groupDTO.getUsers());
                    }
                    return template.update(mapper.map(g, Group.class)).then(Mono.just(g));
                })
                .onErrorResume(ex -> Mono.error(new NotFoundException("Failed to update a group")));
    }
}



