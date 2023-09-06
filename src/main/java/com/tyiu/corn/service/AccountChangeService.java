package com.tyiu.corn.service;

import com.tyiu.corn.exception.NotFoundException;
import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.model.entities.Temporary;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.ChangeRequest;
import com.tyiu.corn.model.requests.UserInfoRequest;
import com.tyiu.corn.model.responses.ChangeResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.model.responses.UserInfoResponse;
import com.tyiu.corn.repository.AccountChangeRepository;
import com.tyiu.corn.repository.UserRepository;

import java.util.*;

import lombok.RequiredArgsConstructor;

import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.MailSendException;
import org.springframework.mail.SimpleMailMessage;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
@EnableScheduling
public class AccountChangeService {
    @Autowired
    private final JavaMailSender emailSender;
    private final AccountChangeRepository accountChangeRepository;
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;

    private void sendEmail(String toAdresses, String subject, String message){
        SimpleMailMessage simpleMailMessage = new SimpleMailMessage();
        simpleMailMessage.setTo(toAdresses);
        simpleMailMessage.setSubject(subject);
        simpleMailMessage.setText(message);
        this.emailSender.send(simpleMailMessage);
    }

    public Flux<Void> sendInvitations(InvitationDTO invitations) throws MailSendException, NotFoundException {
        /*if (invitations.getRoles() == null){

        }*/
        Flux<String> inv = Flux.fromIterable(invitations.getEmails());
        return inv.flatMap(e -> {
            Mono<Boolean> isExists = userRepository.existsByEmail(e).flatMap(
                    b -> {
                        if(!b){
                            Date date = new Date();
                            long milliseconds = date.getTime() + 259200000;
                            date.setTime(milliseconds);
                            Temporary invitation = new Temporary();
                            invitation.setRoles(invitations.getRoles());
                            invitation.setEmail(e);
                            invitation.setDateExpired(date);
                            invitation.setUrl(UUID.randomUUID().toString());
                            sendEmail(
                                    invitation.getEmail(),
                                    "Приглашение",
                                    String.format("Приглашение на регистрацию http://localhost:8080/register/%s", invitation.getUrl())
                            );
                            accountChangeRepository.save(invitation);
                        }
                        return Mono.empty();
                    });
            return Flux.empty();
        });
    }

    public void sendInvitation(Temporary invitation) throws  NotFoundException{
        /*if (userRepository.existsByEmail(invitation.getEmail())){

        }*/
        Mono<String> inv = Mono.just(invitation.getEmail());
        inv.flatMap(e -> {
            accountChangeRepository.existsByEmail(e).flatMap(b -> {
                    UUID url = UUID.randomUUID();
                    Date date = new Date();
                    long milliseconds = date.getTime() + 259200000;
                    date.setTime(milliseconds);
                    invitation.setRoles(List.of(Role.INITIATOR));
                    invitation.setDateExpired(date);
                    invitation.setUrl(url.toString());
                    sendEmail(
                            invitation.getEmail(),
                            "Приглашение",
                            String.format("Приглашение на регистрацию http://localhost:8080/register/%s", invitation.getUrl())
                    );
                    if (b){
                        accountChangeRepository.deleteByEmail(e);
                    }
                    accountChangeRepository.save(invitation).subscribe();
                    return Mono.empty();
            }).subscribe();
            return Mono.empty();
        }).subscribe();
    }

    public Mono<InvitationResponse> findByUrl(String url) {
        Mono<Temporary> invitation = accountChangeRepository.findByUrl(url);
        return invitation.flatMap(
                i -> Mono.just(
                        InvitationResponse.builder()
                                .email(i.getEmail())
                                .roles(i.getRoles())
                                .build()
                )
        ).cast(InvitationResponse.class);
    }

    public Mono<ChangeResponse> findByUrlAndSendCode(String url) {
        Mono<Temporary> emailChange = accountChangeRepository.findByUrl(url);
        /*if (userRepository.existsByEmail(emailChange.getNewEmail())){

        }*/
        return emailChange.flatMap(e -> {
            sendEmail(
                    e.getOldEmail(),
                    "Код для изменения почты",
                    String.format("Введите этот код для изменения почты %d", e.getCode())
            );
            return Mono.just(
                    ChangeResponse.builder()
                            .newEmail(e.getNewEmail())
                            .oldEmail(e.getOldEmail())
                            .build()
            );
        });
    }

    public void sendEmailToChangeEmail(Temporary emailChange){
        /*if (userRepository.existsByEmail(emailChange.getNewEmail())){

        }*/
        Mono<String> ema = Mono.just(emailChange.getOldEmail());
        ema.flatMap(e -> {
            accountChangeRepository.existsByOldEmail(e).flatMap(b -> {
                if (b){
                    accountChangeRepository.deleteByOldEmail(e);
                }
                Date date = new Date();
                date.setTime(date.getTime()+43200000);
                emailChange.setUrl(UUID.randomUUID().toString());
                emailChange.setDateExpired(date);
                emailChange.setCode(new Random(System.currentTimeMillis()).nextInt(900000)+100000);
                sendEmail(
                        emailChange.getNewEmail(),
                        "Изменение почты",
                        String.format("Ссылка для смены почты: http://localhost:8080/change-email/%s", emailChange.getUrl())
                );
                accountChangeRepository.save(emailChange).subscribe();
                return Mono.empty();
            }).subscribe();
            return Mono.empty();
        }).subscribe();
    }

    public Mono<String> sendEmailToChangePassword(Temporary passwordChange) {
        /*if (!userRepository.existsByEmail(passwordChange.getEmail())) {

        }*/
        Date date = new Date();
        date.setTime(date.getTime() + 300000);
        passwordChange.setDateExpired(date);
        passwordChange.setUrl(UUID.randomUUID().toString());
        passwordChange.setCode(new Random(System.currentTimeMillis()).nextInt(900000) + 100000);
        sendEmail(
                passwordChange.getEmail(),
                "Восстановление пароля",
                String.format("Введите этот код для восстановления пароля: %d", passwordChange.getCode()));
        accountChangeRepository.save(passwordChange).subscribe();
        return Mono.just(passwordChange.getUrl());
    }

    public void changePasswordByUser(ChangeRequest request){
        Mono<Temporary> changePassword = accountChangeRepository.findByUrl(request.getKey());
        changePassword.flatMap(c -> {
            if (new Date().getTime() > c.getDateExpired().getTime()){
                accountChangeRepository.deleteByUrl(c.getUrl());
            }
            if (request.getCode() == c.getCode()) {
                Mono<User> user = userRepository.findFirstByEmail(c.getEmail());
                user.flatMap(u -> {
                    userRepository.setPassword(passwordEncoder.encode(request.getPassword()), u.getId());
                    return Mono.empty();
                }).subscribe();
                accountChangeRepository.delete(c).subscribe();
            }
            return Mono.empty();
        }).subscribe();
    }


    public void changeEmailByUser(ChangeRequest request){
        Mono<Temporary> emailChange = accountChangeRepository.findByUrl(request.getUrl());
        emailChange.flatMap(e -> {
            if (request.getCode() == e.getCode()){
                Mono<User> user = userRepository.findFirstByEmail(request.getOldEmail());
                user.flatMap(u -> {
                    userRepository.setEmail(request.getNewEmail(), u.getId());
                    return Mono.empty();
                }).subscribe();
                accountChangeRepository.delete(e).subscribe();
            }
            return Mono.empty();
        }).subscribe();
    }

    public Flux<UserInfoResponse> getUsersInfo(){
        Flux<User> users = userRepository.findAll();
        return users.flatMap(
                u -> Mono.just(UserInfoResponse.builder()
                        .email(u.getEmail())
                        .roles(u.getRoles())
                        .firstName(u.getFirstName())
                        .lastName(u.getLastName())
                        .build())
        ).cast(UserInfoResponse.class);
    }

    public Flux<String> getAllEmails(){
        Flux<User> users = userRepository.findAll();
        return users.flatMap(
                u -> Mono.just(u.getEmail())
        ).cast(String.class);
    }

    public void changeUserInfo(UserInfoRequest request){
        Mono<User> user = userRepository.findFirstByEmail(request.getEmail());
        user.flatMap(u -> {
            userRepository.setUserInfo(
                    request.getNewEmail(),
                    request.getNewFirstName(),
                    request.getNewLastName(),
                    request.getNewRoles(),
                    u.getId());
            return Mono.empty();
        }).subscribe();
    }

    public void deleteDataByUrl(String url){
        accountChangeRepository.deleteByUrl(url);
    }

    @Scheduled(fixedRate = 43200000)
    public void deleteExpiredData(){
        Date date = new Date();
        accountChangeRepository.deleteExpiredData(date);
    }
}
