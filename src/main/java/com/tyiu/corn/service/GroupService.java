package com.tyiu.corn.service;

import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.mappers.GroupMapper;
import com.tyiu.corn.model.entities.mappers.UserMapper;
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

import java.util.ArrayList;
import java.util.List;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "groups")
public class GroupService {
    private final GroupMapper groupMapper;
    private final UserMapper userMapper;
    private final R2dbcEntityTemplate template;
    private final ModelMapper mapper;

    @Cacheable
    public Flux<GroupDTO> getGroups() {
        return template.select(Group.class).all()
                .flatMap(g -> Flux.just(mapper.map(g, GroupDTO.class)));
    }
    @Cacheable
    public Mono<GroupDTO> getGroupById(String groupId) {
        return getGroup(groupId);
    }

    @CacheEvict(allEntries = true)
    public Mono<GroupDTO> createGroup(GroupDTO groupDTO) {
        Group group = mapper.map(groupDTO, Group.class);
        return template.insert(group).flatMap(g -> {
            groupDTO.setId(g.getId());
            return Flux.fromIterable(groupDTO.getUsers()).flatMap(u ->
                    template.insert(new Group2User(u.getId(), g.getId())
                    )).then();
        }).thenReturn(groupDTO);
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteGroup(String id) {
        return template.delete(query(where("id").is(id)), Group.class).then();
    }

    @CacheEvict(allEntries = true)
    public Mono<GroupDTO> updateGroup(String groupId, GroupDTO groupDTO) {
        return getGroup(groupId)
                .flatMap(g -> {
                    groupDTO.setId(g.getId());
                    return template.delete(query(where("group_id").is(groupId)),Group2User.class)
                            .thenReturn(groupDTO.getUsers())
                            .map(list -> {
                                list.forEach(u -> template.insert(new Group2User(u.getId(), groupId)).subscribe());
                                return list;
                            }).thenReturn(groupDTO);
                });
    }

    private Mono<GroupDTO> getGroup(String groupId){
        String query = "SELECT groups.*, users.id user_id, users.email, users.first_name, users.last_name " +
                "FROM groups " +
                "LEFT JOIN group_user ON groups.id = group_user.group_id " +
                "LEFT JOIN users ON group_user.user_id = users.id " +
                "WHERE groups.id = :groupId";
        return template.getDatabaseClient()
                .sql(query)
                .bind("groupId", groupId)
                .flatMap(d -> {
                    List<UserDTO> users = new ArrayList<>();
                    return d.map((row, rowMetadata) -> {
                        users.add(userMapper.apply(row,rowMetadata));
                        GroupDTO groupDTO = groupMapper.apply(row,rowMetadata);
                        groupDTO.setUsers(users);
                        return groupDTO;
                    });
                }).last();
    }
}



