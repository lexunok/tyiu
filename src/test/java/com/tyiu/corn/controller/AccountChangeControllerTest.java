package com.tyiu.corn.controller;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.NoSuchElementException;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.reactive.server.WebTestClient;

import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.model.entities.Temporary;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.ChangeRequest;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.requests.UserInfoRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.ChangeResponse;
import com.tyiu.corn.model.responses.ErrorResponse;
import com.tyiu.corn.model.responses.InvitationResponse;
import com.tyiu.corn.repository.AccountChangeRepository;
import com.tyiu.corn.service.AccountChangeService;

import reactor.core.publisher.Mono;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@WebFluxTest(controllers = AccountChangeController.class)
@AutoConfigureWebTestClient(timeout = "100000")//100 seconds
public class AccountChangeControllerTest {
    @Autowired
    private WebTestClient webTestClient;

    private String jwt;

    @Autowired
    private AccountChangeRepository accountChangeRepository;

    @Autowired
    private AccountChangeService accountChangeService;

    @BeforeAll
    void setUp(){
        RegisterRequest request = new RegisterRequest(
                "dontfakemail@gmfefail.com","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        jwt = response.getToken();
    }

    @Test
    @SuppressWarnings("unchecked")
    void sendRightListEmails(){
        List<String> emails = List.of(
            "wgweg@gfefemail.com",
            "awgweg@gfefemail.com",
            "bawgweg@gfefemail.com",
            "cbadwfggweg@gfefemail.com",
            "cbadwwdgweg@gfefemail.com",
            "cbadwefgweg@gfefemail.com",
            "cbadwf3gweg@gfefemail.com",
            "cbadwgwerfrg@gfefemail.com",
            "cbadwgweg@gfefemail.com"
        );
        InvitationDTO request = InvitationDTO.builder()
        .emails(emails)
        .roles(List.of(Role.ADMIN, Role.EXPERT))
        .build();
        
        Map<String, String> response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/emails")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), InvitationDTO.class)
            .exchange().expectBody(Map.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Успешное приглашение", response.get("success"));
    }

    @Test
    void sendRightOneEmail(){
        Temporary request = Temporary.builder()
        .email("timur.minyazeff@gmail.com")
        .build();

        Map<String, String> response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(new ParameterizedTypeReference<Map<String, String>>() {})
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Успешное приглашение", response.get("success"));
    }
    @Test
    void findInvitationByUrlIfExist(){
        String request = UUID.randomUUID().toString();
        Date date = new Date();
        long milsec = date.getTime() + 259200000;
        date.setTime(milsec);
        Temporary invitation = Temporary.builder()
                .email("Emailssd")
                .roles(List.of(Role.ADMIN))
                .url(request)
                .dateExpired(date)
                .build();
        accountChangeRepository.save(invitation);
        
        InvitationResponse response = webTestClient
        .get()
        .uri("/api/v1/profile-action/get/invitation/{request}", request)
        .exchange()
        .expectBody(InvitationResponse.class)
        .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(invitation.getEmail(), response.getEmail());
        assertEquals(invitation.getRoles(), response.getRoles());
    }
    @Test
    void findInvitationByUrlIfNotExist(){
        String request = UUID.randomUUID().toString();
        
        ErrorResponse response = webTestClient
        .get()
        .uri("/api/v1/profile-action/get/invitation/{request}", request)
        .exchange()
        .expectBody(ErrorResponse.class)
        .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals("Приглашения " + request + " не существует", response.getError());
    }
    @Test
    void catchEmailSenderException(){
        Temporary request = Temporary.builder()
        .email("ideasmanager")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте домен почты", response.getError());
    }
    @Test
    void catchEmailParseException(){
        Temporary request = Temporary.builder()
        .email("@gmail.com")
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте имя пользователя почты", response.getError());
    }
    @Test
    void catchNotFoundExceptionWithoutEmail(){
        Temporary request = Temporary.builder()
        .roles(List.of(Role.ADMIN, Role.PROJECT_OFFICE))
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте почту", response.getError());
    }
    @Test
    void catchNotFoundExceptionWithEmptyBody(){
        Temporary request = Temporary.builder()
        .build();

        ErrorResponse response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(request), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();
        
        assertNotNull(response);
        assertEquals("Добавьте почту", response.getError());
    }
    @Test
    void sendRightRequestToResetEmail(){
        Temporary emailChange = Temporary.builder()
        .oldEmail("dontfakemail@gmfefail.com")
        .newEmail("fakeermail@gmrail.com")
        .build();

        Map<String, String> response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/request-to-change-email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(emailChange), Temporary.class)
            .exchange().expectBody(new ParameterizedTypeReference<Map<String, String>>() {})
            .returnResult().getResponseBody();

        assertNotNull(response);
        assertEquals(1,response.size());
        assertEquals("Ссылка на изменение почты находится на новой почте", response.get("success"));
    }
    @Test
    void sendRightRequestToResetPassword(){
        Temporary passwordChange = Temporary.builder()
        .email("dontfakemail@gmfefail.com")
        .build();

        Map<String, String> response = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/request-to-change-password")
            .body(Mono.just(passwordChange), Temporary.class)
            .exchange().expectBody(new ParameterizedTypeReference<Map<String, String>>() {})
            .returnResult().getResponseBody();

        assertNotNull(response);
        assertNotNull(response.get("key"));
    }
    @Test
    void sendFailRequestToResetPasswordWhenEmailNotExist(){
        Temporary passwordChange = Temporary.builder()
        .email("dontefakeewegmail@gmadfirfl.com")
        .build();

        ErrorResponse errorResponse = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/request-to-change-password")
            .body(Mono.just(passwordChange), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();

        assertNotNull(errorResponse);
        assertEquals(errorResponse.getError(), "Пользователя с почтой dontefakeewegmail@gmadfirfl.com не существует");
    }
    @Test
    void sendRightRequestToResetEmailWhenEmailIsUsing(){
        Temporary emailChange = Temporary.builder()
        .oldEmail("12345@gmail.com")
        .newEmail("dontfakemail@gmfefail.com")
        .build();

        ErrorResponse errorResponse = webTestClient
            .post()
            .uri("/api/v1/profile-action/send/request-to-change-email")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(emailChange), Temporary.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();

        assertNotNull(errorResponse);
        assertEquals(errorResponse.getError(), "Пользователь с такой почтой существует");
    }
    @Test
    void changePassword(){
        //Before
        RegisterRequest request = new RegisterRequest(
                "123456@gmaergrfefifl.com","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        Date date = new Date();
        date.setTime(date.getTime() + 300000);
        Temporary emailChange = Temporary.builder()
        .email("123456@gmaergrfefifl.com")
        .code(123456)
        .url("12345678")
        .dateExpired(date)
        .build();
        accountChangeRepository.save(emailChange);

        //Test
        ChangeRequest requestChange = new ChangeRequest(
            null,
            null, 
            null, 
            123456, 
            "123456@gmaergrfefifl.com", 
            "fakepass2",
            "12345678");

        Map<String, String> responseChange = webTestClient
            .put()
            .uri("/api/v1/profile-action/change/password")
            .body(Mono.just(requestChange), ChangeRequest.class)
            .exchange().expectBody(new ParameterizedTypeReference<Map<String, String>>() {})
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals("Успешное изменение пароля", responseChange.get("success"));
    }
    @Test
    void changeEmail(){
        //Before
         RegisterRequest request = new RegisterRequest(
                "123456@gmdaifl.com","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        Date date = new Date();
        date.setTime(date.getTime() + 300000);
        Temporary emailChange = Temporary.builder()
        .oldEmail("123456@gmdaifl.com")
        .newEmail("1234567@gmdaifl.com")
        .code(123456)
        .url("12345")
        .dateExpired(date)
        .build();
        accountChangeRepository.save(emailChange);

        //Test
        ChangeRequest requestChange = new ChangeRequest(
            "1234567@gmdaifl.com",
            "123456@gmdaifl.com", 
            "12345", 
            123456, 
            null, 
            null,
            null);

        Map<String, String> responseChange = webTestClient
            .put()
            .uri("/api/v1/profile-action/change/email")
            .header("Authorization","Bearer " + response.getToken())
            .body(Mono.just(requestChange), ChangeRequest.class)
            .exchange().expectBody(new ParameterizedTypeReference<Map<String, String>>() {})
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals("Успешное изменение почты", responseChange.get("success"));
    }
    @Test
    void changePasswordInvalidCode(){
        //Before
        RegisterRequest request = new RegisterRequest(
                "123456@gmaefefifl.com","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        Date date = new Date();
        date.setTime(date.getTime() + 300000);
        Temporary emailChange = Temporary.builder()
        .email("123456@gmaefefifl.com")
        .code(123456)
        .url("1234567")
        .dateExpired(date)
        .build();
        accountChangeRepository.save(emailChange);

        //Test
        ChangeRequest requestChange = new ChangeRequest(
            null,
             null, 
             null, 
             1234567, 
             "123456@gmaefefifl.com", 
             "fakepass2",
             "1234567");

        ErrorResponse responseChange = webTestClient
            .put()
            .uri("/api/v1/profile-action/change/password")
            .body(Mono.just(requestChange), ChangeRequest.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals("Неправильный код", responseChange.getError());
    }
    @Test
    void changePasswordWhenExpired(){
        //Before
        RegisterRequest request = new RegisterRequest(
                "1234567@gmaefefifl.com","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        Date date = new Date();
        date.setTime(date.getTime() - 300001);
        Temporary emailChange = Temporary.builder()
        .email("1234567@gmaefefifl.com")
        .code(123456)
        .url("123456")
        .dateExpired(date)
        .build();
        accountChangeRepository.save(emailChange);

        //Test
        ChangeRequest requestChange = new ChangeRequest(
            null,
             null, 
             null, 
             123456, 
             "1234567@gmaefefifl.com", 
             "fakepass2",
             "123456");

        ErrorResponse responseChange = webTestClient
            .put()
            .uri("/api/v1/profile-action/change/password")
            .body(Mono.just(requestChange), ChangeRequest.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals("Время действия кода истекло", responseChange.getError());
        assertThrows(NoSuchElementException.class, () -> {
            accountChangeRepository.findByUrl(requestChange.getUrl()).get();
        });
    }
    @Test
    void changeEmailInvalidCode(){
        //Before
         RegisterRequest request = new RegisterRequest(
                "123456@gmdaewegwegifl.com","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        Date date = new Date();
        date.setTime(date.getTime() + 300000);
        Temporary emailChange = Temporary.builder()
        .oldEmail("123456@gmdaewegwegifl.com")
        .newEmail("1234567@gmdaifl.com")
        .code(123456)
        .url("12345d")
        .dateExpired(date)
        .build();
        accountChangeRepository.save(emailChange);

        //Test
        ChangeRequest requestChange = new ChangeRequest(
            "123456@gmdaewegwegifl.com",
            "123456@gmdaifl.com", 
            "12345d", 
            12345, 
            null, 
            null,
            null);

        ErrorResponse responseChange = webTestClient
            .put()
            .uri("/api/v1/profile-action/change/email")
            .header("Authorization","Bearer " + response.getToken())
            .body(Mono.just(requestChange), ChangeRequest.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals(responseChange.getError(), "Неправильный код");
    }
    @Test
    void changeEmailWhenExpired(){
        //Before
         RegisterRequest request = new RegisterRequest(
                "123456@gmdaifl.com","fakename","fakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        Date date = new Date();
        date.setTime(date.getTime() + 44200000);
        Temporary emailChange = Temporary.builder()
        .oldEmail("123456@gmdaifl.com")
        .newEmail("1234567@gmdaifl.com")
        .code(123456)
        .url("12345s")
        .dateExpired(date)
        .build();
        accountChangeRepository.save(emailChange);
        accountChangeService.deleteExpiredData();
        //Test
        ChangeRequest requestChange = new ChangeRequest(
            "1234567@gmdaifl.com",
            "123456@gmdaifl.com", 
            "12345s", 
            12345, 
            null, 
            null,
            null);

        ErrorResponse responseChange = webTestClient
            .put()
            .uri("/api/v1/profile-action/change/email")
            .header("Authorization","Bearer " + response.getToken())
            .body(Mono.just(requestChange), ChangeRequest.class)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals(responseChange.getError(), "Неправильный код");
    }
    @Test
    void changeFullUserInfo(){
        //Before
        RegisterRequest request = new RegisterRequest(
                "old123456saka@gmdaifl.com","oldfakename","oldfakename","fakepass", List.of(Role.ADMIN));
        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);
        //Test
        UserInfoRequest changeRequest = UserInfoRequest.builder()
        .email("old123456saka@gmdaifl.com")
        .newEmail("new123456saka@gmdaifl.com")
        .newFirstName("newfakename")
        .newLastName("newfakename")
        .newRoles(List.of(Role.ADMIN, Role.EXPERT))
        .build();

        Map<String,String> responseChange = webTestClient
            .put()
            .uri("/api/v1/profile-action/change/user-info")
            .header("Authorization","Bearer " + jwt)
            .body(Mono.just(changeRequest), UserInfoRequest.class)
            .exchange().expectBody(new ParameterizedTypeReference<Map<String, String>>() {})
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals(responseChange.get("success"), "Успешное изменение пользователя");
    }
    @Test
    void getEmailChangeData(){
        //Before
        Temporary emailChange = Temporary.builder()
        .oldEmail("dontfakeedemail@gmfefail.com")
        .newEmail("123456@yandxdf.com")
        .url("1234567890")
        .code(123478)
        .build();
        accountChangeRepository.save(emailChange);
        //Test
        String urlParam = emailChange.getUrl();
        ChangeResponse responseChange = webTestClient
            .get()
            .uri("/api/v1/profile-action/change/email/{urlParam}", urlParam)
            .header("Authorization","Bearer " + jwt)
            .exchange().expectBody(ChangeResponse.class)
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals(responseChange.getNewEmail(), "123456@yandxdf.com");
        assertEquals(responseChange.getOldEmail(), "dontfakeedemail@gmfefail.com");
    }
    @Test
    void getEmailChangeDataThatNotExist(){
        //Test
        ErrorResponse responseChange = webTestClient
            .get()
            .uri("/api/v1/profile-action/change/email/eer1234urlparamf")
            .header("Authorization","Bearer " + jwt)
            .exchange().expectBody(ErrorResponse.class)
            .returnResult().getResponseBody();

        assertNotNull(responseChange);
        assertEquals(responseChange.getError(), "Доступ зарпрещен");
    }
}
