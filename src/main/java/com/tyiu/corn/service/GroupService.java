package com.tyiu.corn.service;

import com.tyiu.corn.config.exception.ErrorException;
import com.tyiu.corn.model.dto.GroupDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.User;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import com.tyiu.corn.model.entities.Group;
import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.List;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "groups")
public class GroupService {

    private final ReactiveMongoTemplate template;
    private final ModelMapper mapper;

    @Cacheable
    public Flux<GroupDTO> getGroups() {
        Flux<Group> groups = template.findAll(Group.class);
        return groups.flatMap(g -> Flux.just(mapper.map(g, GroupDTO.class)))
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    @Cacheable
    public Mono<GroupDTO> getGroupById(String id) {
        Mono<Group> group = template.findById(id, Group.class);
        return group.flatMap(g -> {
            GroupDTO groupDTO = mapper.map(g, GroupDTO.class);
            Flux<User> users = template
                    .find(Query.query(Criteria.where("_id")
                    .in(g.getUsersId())), User.class);
            return users.flatMap(u -> Flux.just(mapper.map(u, UserDTO.class))).collectList()
                    .flatMap(list -> {
                        groupDTO.setUsers(list);
                        return Mono.just(groupDTO);
                    });
        }).onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    @CacheEvict(allEntries = true)
    public Mono<GroupDTO> createGroup(GroupDTO groupDTO) {
        Group group = mapper.map(groupDTO, Group.class);
        List<String> usersId = groupDTO.getUsers().stream().map(UserDTO::getId).toList();
        group.setUsersId(usersId);
        return template.save(group).flatMap(g -> Mono.just(mapper.map(g, GroupDTO.class)))
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> updateGroup(String id,GroupDTO groupDTO) {
        return template.findById(id, Group.class)
                .flatMap(g -> {
                    g.setName(groupDTO.getName());
                    g.setUsersId(groupDTO.getUsers().stream().map(UserDTO::getId).toList());
                    g.setRoles(groupDTO.getRoles());
                    return template.save(g);
                }).then().onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
    @CacheEvict(allEntries = true)
    public Mono<Void> deleteGroup(String id) {
        return template.remove(Query.query(Criteria.where("_id").is(id)), Group.class).then()
                .onErrorResume(ex -> Mono.error(new ErrorException("Not success!")));
    }
}



