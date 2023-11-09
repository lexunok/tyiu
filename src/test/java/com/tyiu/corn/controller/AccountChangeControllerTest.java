package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.InvitationDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.Temporary;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class AccountChangeControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;

    private String jwt;
    private UserDTO userDTO;

    private String EMAIL = "vshtst.hits0@gmail.com";

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "account.change@gmail.com", "account", "change", "account.change", List.of(Role.ADMIN));

        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);

        jwt = response.getToken();

        userDTO = UserDTO.builder()
                .id(response.getId())
                .email(response.getEmail())
                .lastName(response.getLastName())
                .firstName(response.getFirstName())
                .roles(response.getRoles())
                .build();
    }

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Test
    void testRegisterByInvitation(){
        Temporary temporary = Temporary.builder()
                .email(EMAIL)
                .build();
        String changerUrl = webTestClient
                .post()
                .uri("/api/v1/profile/send/change/password")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(temporary), Temporary.class)
                .exchange()
                .expectBody(String.class)
                .returnResult().getResponseBody();
        assertNotNull(changerUrl);
        assertNotEquals(changerUrl, "Не удалось отправить ссылку на почту");
        InvitationResponse findByUrl = webTestClient
                .get()
                .uri("/api/v1/profile/get/invitation/{url}", changerUrl)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(InvitationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(findByUrl);
        assertEquals(findByUrl.getEmail(), temporary.getEmail());
    }

    @Test
    void testChangeNewEmail(){
        Temporary temporary = Temporary.builder()
                .email(EMAIL)
                .newEmail(EMAIL)
                .oldEmail(EMAIL)
                .build();
        String changerUrl = webTestClient
                .post()
                .uri("/api/v1/profile/send/change/password")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(temporary), Temporary.class)
                .exchange()
                .expectBody(String.class)
                .returnResult().getResponseBody();
        assertNotNull(changerUrl);
        assertNotEquals(changerUrl, "Не удалось отправить ссылку на почту");
        ChangeResponse findByUrl = webTestClient
                .get()
                .uri("/api/v1/profile/change/email/{url}", changerUrl)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(ChangeResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(findByUrl);
        assertEquals(findByUrl.getNewEmail(), temporary.getNewEmail());
        assertEquals(findByUrl.getOldEmail(), temporary.getOldEmail());
    }

    @Test
    void testGetUsersInfo(){
        List<UserInfoResponse> users = webTestClient
                .get()
                .uri("/api/v1/profile/get/users")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(UserInfoResponse.class)
                .returnResult().getResponseBody();
        UserInfoResponse user = users.get(0);
        assertNotNull(user);
        assertEquals(userDTO.getEmail(), user.getEmail());
    }

    @Test
    void testGetUsersEmail(){
        List<String> users = webTestClient
                .get()
                .uri("/api/v1/profile/get/emails")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(List.class)
                .returnResult().getResponseBody();
        String userEmail = users.get(0);
        assertNotNull(userEmail);
        assertEquals(userDTO.getEmail(), userEmail);
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @Test
    void testInvitationSend(){
        Temporary temporary = Temporary.builder()
                .email(EMAIL)
                .build();
        InfoResponse invitationSend = webTestClient
                .post()
                .uri("/api/v1/profile/send/email")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(temporary), Temporary.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(invitationSend);
        assertEquals(HttpStatus.OK, invitationSend.getStatusCode());
    }

    @Test
    void testInvitationFileSend(){
        InvitationDTO invitationDTO = InvitationDTO.builder()
                .roles(List.of(Role.INITIATOR))
                .emails(List.of(EMAIL))
                .build();
        webTestClient
                .post()
                .uri("/api/v1/profile/send/emails")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(invitationDTO), Temporary.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testRequestToChangeEmail(){
        Temporary temporary = Temporary.builder()
                .oldEmail(EMAIL)
                .newEmail(EMAIL)
                .build();
        InfoResponse invitationSend = webTestClient
                .post()
                .uri("/api/v1/profile/send/change/email")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(temporary), Temporary.class)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(invitationSend);
        assertEquals(HttpStatus.OK, invitationSend.getStatusCode());
    }

    @Test
    void testRequestToChangePassword(){
        Temporary temporary = Temporary.builder()
                .email(EMAIL)
                .build();
        String changerUrl = webTestClient
                .post()
                .uri("/api/v1/profile/send/change/password")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(temporary), Temporary.class)
                .exchange()
                .expectBody(String.class)
                .returnResult().getResponseBody();
        assertNotNull(changerUrl);
        assertNotEquals(changerUrl, "Не удалось отправить ссылку на почту");
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @Test
    void testDeleteByUrl(){
        Temporary temporary = Temporary.builder()
                .email(EMAIL)
                .build();
        String changerUrl = webTestClient
                .post()
                .uri("/api/v1/profile/send/change/password")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(temporary), Temporary.class)
                .exchange()
                .expectBody(String.class)
                .returnResult().getResponseBody();
        assertNotNull(changerUrl);
        assertNotEquals(changerUrl, "Не удалось отправить ссылку на почту");
        InfoResponse delete = webTestClient
                .get()
                .uri("/api/v1/profile/delete/invitation/{url}", changerUrl)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(delete);
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @Test
    void testChangePasswordByUser(){
        UserDTO newUserDTO = UserDTO.builder()
                .id(userDTO.getId())
                .email("change.account@gmail.com")
                .firstName(userDTO.getFirstName())
                .lastName(userDTO.getLastName())
                .roles(userDTO.getRoles())
                .build();
        UserDTO changeUser = webTestClient
                .post()
                .uri("/api/v1/profile/change/info")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(newUserDTO), UserDTO.class)
                .exchange()
                .expectBody(UserDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(changeUser);
        assertNotEquals(changeUser.getEmail(), userDTO.getEmail());
    }

}
