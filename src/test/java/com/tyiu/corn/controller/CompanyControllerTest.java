package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.CompanyDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

import reactor.core.publisher.Mono;

import java.util.List;


@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class CompanyControllerTest extends TestContainers{

    @Autowired
    private WebTestClient webTestClient;
    private UserDTO userDTO;
    private String jwt;
    private CompanyDTO createCompany() {
        CompanyDTO company = CompanyDTO.builder()
                .name("company")
                .users(List.of(userDTO))
                .owner(userDTO)
                .build();
        return webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "fakemail72", "fakename", "fakename", "fakepass", List.of(Role.ADMIN));

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

    @Test
    void testCreateCompany() {
        CompanyDTO createdCompany = createCompany();
        assertEquals("company", createdCompany.getName());
    }

    @Test
    void testUpdateCompany() {
        CompanyDTO company = CompanyDTO.builder()
                .name("company1")
                .users(List.of(userDTO))
                .owner(userDTO)
                .build();

        CompanyDTO responseCreateCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany);
        assertEquals(company.getName(), responseCreateCompany.getName());
        Long id = responseCreateCompany.getId();

        company = CompanyDTO.builder()
                .name("company1Updated")
                .users(List.of(userDTO))
                .owner(userDTO)
                .build();

        webTestClient
                .put()
                .uri("/api/v1/company/update/{id}", id)
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testDeleteCompany() {
        CompanyDTO company = CompanyDTO.builder()
                .name("company")
                .users(List.of(userDTO))
                .owner(userDTO)
                .build();

        CompanyDTO responseDeleteCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseDeleteCompany);
        assertEquals(company.getName(), responseDeleteCompany.getName());
        Long id = responseDeleteCompany.getId();

        webTestClient
                .delete()
                .uri("/api/v1/company/delete/{id}", id)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testGetCompanyById() {
        CompanyDTO company = CompanyDTO.builder()
                .name("company")
                .users(List.of(userDTO))
                .owner(userDTO)
                .build();

        CompanyDTO responseCreateCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany);
        assertEquals(company.getName(), responseCreateCompany.getName());
        Long id = responseCreateCompany.getId();

        CompanyDTO responseGetCompany = webTestClient
                .get()
                .uri("/api/v1/company/{id}", id)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetCompany);
        assertEquals(company.getName(), responseGetCompany.getName());
    }

    @Test
    void testGetCompanyList() {
        CompanyDTO company1 = CompanyDTO.builder()
                .name("company 1")
                .users(List.of(userDTO))
                .owner(userDTO)
                .build();

        CompanyDTO responseCreateCompany1 = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company1), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();

        CompanyDTO company2 = CompanyDTO.builder()
                .name("company 2")
                .users(List.of(userDTO))
                .owner(userDTO)
                .build();

        CompanyDTO responseCreateCompany2 = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company2), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();

        List<CompanyDTO> listCompany = webTestClient
                .get()
                .uri("api/v1/company/all")
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(listCompany);
    }

    @Test
    void testGetCompanyStaff() {
        RegisterRequest request = new RegisterRequest("user@mail.com", "firstname", "lastname", "password", List.of(Role.ADMIN));

        AuthenticationResponse user = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(user);

        UserDTO userDTO = UserDTO.builder()
                .id(user.getId())
                .email(user.getEmail())
                .lastName(user.getLastName())
                .firstName(user.getFirstName())
                .roles(user.getRoles())
                .build();

        CompanyDTO company = CompanyDTO.builder()
                .name("company 1")
                .users(List.of(userDTO))
                .owner(userDTO)
                .build();

        webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange();

        webTestClient
                .get()
                .uri("/api/v1/company/all/{userId}", user.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult();
    }
}
