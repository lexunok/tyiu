package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.entities.mappers.GroupMapper;
import com.tyiu.corn.model.entities.relations.Group2User;
import com.tyiu.corn.model.enums.Role;
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
import static org.springframework.data.relational.core.query.Update.update;

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
    public Mono<GroupDTO> getGroupById(Long groupId) {
        String query = "SELECT groups.*, users.id AS member_id, users.email, users.first_name, users.last_name " +
                        "FROM groups " +
                        "LEFT JOIN group_user ON groups.id = group_user.group_id " +
                        "LEFT JOIN users ON group_user.user_id = users.id " +
                        "WHERE groups.id = :groupId";
        GroupMapper groupMapper = new GroupMapper();
        return template.getDatabaseClient()
                .sql(query)
                .bind("groupId", groupId)
                .map(groupMapper::apply)
                .all()
                .collectList()
                .map(groupDTOMap -> groupDTOMap.get(0))
        .switchIfEmpty(Mono.error(new ErrorException("Failed to get a group")));
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
        }).switchIfEmpty(Mono.error(new ErrorException("Failed to create a group")));
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteGroup(Long id) {
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
    public Mono<Void> addInGroup(Long groupId, Long userId) {
        return template.insert(new Group2User(userId, groupId)).then()
        .onErrorResume(ex -> Mono.error(new ErrorException("Failed to add in group")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> addListInGroup(Long groupId, List<Long> usersId) {
        usersId.forEach(id -> template.insert(new Group2User(id, groupId)).subscribe());
        return Mono.empty();
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> kickFromGroup(Long groupId, Long userId) {
        return template.delete(query(where("userId").is(userId)
                        .and("groupId").is(groupId)), Group2User.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to kick from group")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateGroup(Long groupId,GroupDTO groupDTO) {
        return template.update(query(where("id").is(groupId)),
                update("name", groupDTO.getName())
                        .set("roles", groupDTO.getRoles().stream().map(Role::name).toArray(String[]::new)),
                Group.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Failed to update a group")));
    }
}



