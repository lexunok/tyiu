package com.tyiu.ideas.service;

import com.tyiu.ideas.model.dto.GroupDTO;
import com.tyiu.ideas.model.dto.UserDTO;
import com.tyiu.ideas.model.entities.User;
import com.tyiu.ideas.model.entities.mappers.GroupMapper;
import com.tyiu.ideas.model.entities.mappers.UserMapper;
import com.tyiu.ideas.model.entities.relations.Group2User;
import com.tyiu.ideas.publisher.NotificationPublisher;
import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.stereotype.Service;
import com.tyiu.ideas.model.entities.Group;
import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import request.NotificationRequest;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;
import static org.springframework.data.relational.core.query.Update.update;

@Service
@RequiredArgsConstructor
@CacheConfig(cacheNames = "groups")
public class GroupService {

    private final ModelMapper mapper;
    private final UserMapper userMapper;
    private final GroupMapper groupMapper;
    private final R2dbcEntityTemplate template;
    private final NotificationPublisher notificationPublisher;

    private Mono<Void> sendNotificationOnCreateGroup(String groupName, String userId, User user) {

        Mono.fromRunnable(() -> template.selectOne(query(where("id").is(userId)), User.class)
                .flatMap(userThatAddedInGroup -> notificationPublisher.makeNotification(

                        NotificationRequest.builder()
                                .publisherEmail(user.getEmail())
                                .consumerEmail(userThatAddedInGroup.getEmail())
                                .title("Вы вошли в состав пользовательской группы")
                                .message(String.format(
                                        "Админ %s %s сделал вас участником пользовательской группы \"%s\".",
                                        user.getFirstName(),
                                        user.getLastName(),
                                        groupName
                                ))
                                .build())
                ).publishOn(Schedulers.boundedElastic()).subscribe());

        return Mono.empty();
    }

    private void sendNotificationOnDeleteGroup(String groupName, String groupId, User user) {

        Mono.fromRunnable(() -> template.select(query(where("group_id").is(groupId)), Group2User.class)
                .flatMap(group2User -> template.selectOne(query(where("id").is(group2User.getUserId())), User.class))
                .flatMap(userToNotify -> notificationPublisher.makeNotification(

                        NotificationRequest.builder()
                                .publisherEmail(user.getEmail())
                                .consumerEmail(userToNotify.getEmail())
                                .title("Группа, в которой вы состояли, была удалена")
                                .message(String.format(
                                        "Админ %s %s удалил группу \"%s\".",
                                        user.getFirstName(),
                                        user.getLastName(),
                                        groupName
                                ))
                                .build()
                ))
                .publishOn(Schedulers.boundedElastic())
                .subscribe());
    }

    private Mono<Void> sendNotificationToGroupUsers(GroupDTO groupDTO, User admin) {

        template.select(query(where("group_id").is(groupDTO.getId())), Group2User.class)
                .flatMap(group2User -> Mono.just(group2User.getUserId()))
                .collectList()
                .map(currentUserIdlist -> template.delete(query(where("group_id").is(groupDTO.getId())), Group2User.class)
                        .flatMap(d -> {

                            List<String> newId = groupDTO.getUsers().stream().map(UserDTO::getId).toList();
                            Flux.fromIterable(newId.stream()
                                            .distinct().filter(currentUserIdlist::contains)
                                            .collect(Collectors.toSet()))
                                    .flatMap(idToStay -> template.insert(new Group2User(idToStay, groupDTO.getId())))
                                    .subscribe();

                            Flux.fromIterable(newId.stream()
                                            .distinct().filter(id -> !currentUserIdlist.contains(id))
                                            .collect(Collectors.toSet()))
                                    .flatMap(idToAdd -> template.insert(new Group2User(idToAdd, groupDTO.getId()))
                                            .then(template.selectOne(query(where("id").is(idToAdd)), User.class)))
                                    .flatMap(user -> notificationPublisher.makeNotification(

                                            NotificationRequest.builder()
                                                    .publisherEmail(admin.getEmail())
                                                    .consumerEmail(user.getEmail())
                                                    .title("Вы вошли в состав пользовательской группы")
                                                    .message(String.format(
                                                            "Админ %s %s сделал вас участником пользовательской группы \"%s\".",
                                                            admin.getFirstName(),
                                                            admin.getLastName(),
                                                            groupDTO.getName()
                                                    ))
                                                    .build())
                                    ).subscribe();

                            Flux.fromIterable(currentUserIdlist.stream()
                                            .distinct().filter(id -> !newId.contains(id))
                                            .collect(Collectors.toSet()))
                                    .flatMap(idToDelete -> template.selectOne(query(where("id").is(idToDelete)), User.class))
                                    .flatMap(user -> notificationPublisher.makeNotification(

                                            NotificationRequest.builder()
                                                    .publisherEmail(admin.getEmail())
                                                    .consumerEmail(user.getEmail())
                                                    .title("Вас исключили из состава пользовательской группы")
                                                    .message(String.format(
                                                            "Админ %s %s исключил вас из пользовательской группы \"%s\".",
                                                            admin.getFirstName(),
                                                            admin.getLastName(),
                                                            groupDTO.getName()
                                                    ))
                                                    .build())
                                    ).subscribe();

                            return Mono.empty();
                        })
                ).publishOn(Schedulers.boundedElastic()).subscribe();

        return Mono.empty();
    }

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
    public Mono<GroupDTO> createGroup(GroupDTO groupDTO, User admin) {

        Group group = mapper.map(groupDTO, Group.class);
        return template.insert(group).flatMap(g -> {

            groupDTO.setId(g.getId());
            return Flux.fromIterable(groupDTO.getUsers())
                    .flatMap(u -> template.insert(new Group2User(u.getId(), g.getId()))
                            .then(sendNotificationOnCreateGroup(
                                    groupDTO.getName(),
                                    u.getId(),
                                    admin
                            ))
                    ).then();
        }).thenReturn(groupDTO);
    }

    @CacheEvict(allEntries = true)
    public Mono<Void> deleteGroup(String groupId, User admin) {

        return template.selectOne(query(where("id").is(groupId)), Group.class)
                .flatMap(group -> {

                    sendNotificationOnDeleteGroup(
                            group.getName(),
                            groupId,
                            admin
                    );
                    return template.delete(query(where("id").is(group)), Group.class).then();
                }
        );
    }

    @CacheEvict(allEntries = true)
    public Mono<GroupDTO> updateGroup(String groupId, GroupDTO groupDTO, User admin) {

        return getGroup(groupId)
                .flatMap(group -> {

                    groupDTO.setId(group.getId());
                    return template.update(query(where("id").is(groupId)),
                            update("name", groupDTO.getName()),
                                    Group.class)
                            .then(template.update(query(where("id").is(groupId)),
                                    update("roles", groupDTO
                                            .getRoles()
                                            .stream()
                                            .map(Enum::toString)
                                            .toArray(String[]::new)),
                                    Group.class))
                            .then(sendNotificationToGroupUsers(groupDTO, admin))
                            .thenReturn(groupDTO);
                });
    }

    private Mono<GroupDTO> getGroup(String groupId) {

        String query = "SELECT groups.*, users.id user_id, users.email, users.first_name, users.last_name " +
                "FROM groups " +
                "LEFT JOIN group_user ON groups.id = group_user.group_id " +
                "LEFT JOIN users ON group_user.user_id = users.id " +
                "WHERE groups.id = :groupId";

        return template.getDatabaseClient()
                .sql(query)
                .bind("groupId", groupId)
                .flatMap(group -> {

                    List<UserDTO> users = new ArrayList<>();
                    return group.map((row, rowMetadata) -> {

                        users.add(userMapper.apply(row,rowMetadata));
                        GroupDTO groupDTO = groupMapper.apply(row,rowMetadata);
                        groupDTO.setUsers(users);

                        return groupDTO;
                    });
                }).last();
    }
}
