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
    private UserDTO owner;
    private UserDTO member;
    private String jwt1;
    private String jwt2;
    private CompanyDTO createCompany() {
        return CompanyDTO.builder()
                .name("company")
                .users(List.of(owner))
                .owner(owner)
                .build();
    }

    @BeforeAll
    public void setUp() {

        RegisterRequest request1 = new RegisterRequest(
                "fakemailOWNER", "fakename", "fakename", "fakepass",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR));

        AuthenticationResponse response1 = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request1), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response1);

        jwt1 = response1.getToken();

        RegisterRequest request2 = new RegisterRequest(
                "fakemailMEMBER", "fakename", "fakename", "fakepass",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR));

        AuthenticationResponse response = webTestClient
                .post()
                .uri("/api/v1/auth/register")
                .body(Mono.just(request2), RegisterRequest.class)
                .exchange()
                .expectBody(AuthenticationResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(response);

        jwt2 = response.getToken();

        owner = UserDTO.builder()
                .id(response.getId())
                .email(response.getEmail())
                .lastName(response.getLastName())
                .firstName(response.getFirstName())
                .roles(response.getRoles())
                .build();

        member = UserDTO.builder()
                .id(response.getId())
                .email(response.getEmail())
                .lastName(response.getLastName())
                .firstName(response.getFirstName())
                .roles(response.getRoles())
                .build();
    }

    @Test
    void testCreateCompany() {

        CompanyDTO company = createCompany();

        CompanyDTO responseCreateCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany);
        assertEquals(company.getName(), responseCreateCompany.getName());
    }

    @Test
    void testUpdateCompany() {

        CompanyDTO company = createCompany();

        CompanyDTO responseCreateCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany);
        assertEquals(company.getName(), responseCreateCompany.getName());

        company = CompanyDTO.builder()
                .name("company1Updated")
                .users(List.of(owner, member))
                .owner(owner)
                .build();

        webTestClient
                .put()
                .uri("/api/v1/company/update/{id}", responseCreateCompany.getId())
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testDeleteCompany() {

        CompanyDTO company = createCompany();

        CompanyDTO responseDeleteCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseDeleteCompany);
        assertEquals(company.getName(), responseDeleteCompany.getName());

        webTestClient
                .delete()
                .uri("/api/v1/company/delete/{id}", responseDeleteCompany.getId())
                .header("Authorization", "Bearer " + jwt1)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testGetCompanyById() {

        CompanyDTO company = createCompany();

        CompanyDTO responseCreateCompany = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany);
        assertEquals(company.getName(), responseCreateCompany.getName());

        CompanyDTO responseGetCompany = webTestClient
                .get()
                .uri("/api/v1/company/{id}", responseCreateCompany.getId())
                .header("Authorization", "Bearer " + jwt2)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseGetCompany);
        assertEquals(company.getName(), responseGetCompany.getName());
    }

    @Test
    void testGetCompanyList() {

        CompanyDTO company1 = createCompany();

        CompanyDTO responseCreateCompany1 = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company1), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany1);
        assertEquals(company1.getName(), responseCreateCompany1.getName());

        CompanyDTO company2 = CompanyDTO.builder()
                .name("company 2")
                .users(List.of(member))
                .owner(member)
                .build();

        CompanyDTO responseCreateCompany2 = webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt2)
                .body(Mono.just(company2), CompanyDTO.class)
                .exchange()
                .expectBody(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(responseCreateCompany2);
        assertEquals(company2.getName(), responseCreateCompany2.getName());

        List<CompanyDTO> listCompany = webTestClient
                .get()
                .uri("api/v1/company/all")
                .header("Authorization", "Bearer " + jwt2)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(listCompany);
        assertTrue(listCompany.size() > 1);
    }

    @Test
    void testGetCompanyStaff() {

        CompanyDTO company = CompanyDTO.builder()
                .name("company 1")
                .users(List.of(owner, member))
                .owner(owner)
                .build();

        webTestClient
                .post()
                .uri("/api/v1/company/create")
                .header("Authorization", "Bearer " + jwt1)
                .body(Mono.just(company), CompanyDTO.class)
                .exchange();

        webTestClient
                .get()
                .uri("/api/v1/company/all/{userId}", member.getId())
                .header("Authorization", "Bearer " + jwt2)
                .exchange()
                .expectBodyList(CompanyDTO.class)
                .returnResult();
    }
}
