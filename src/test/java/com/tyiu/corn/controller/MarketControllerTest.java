package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.MarketDTO;
import com.tyiu.corn.model.enums.MarketStatus;
import com.tyiu.corn.model.enums.Role;
import com.tyiu.corn.model.requests.RegisterRequest;
import com.tyiu.corn.model.responses.AuthenticationResponse;
import com.tyiu.corn.model.responses.InfoResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.RANDOM_PORT;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = RANDOM_PORT)
public class MarketControllerTest extends TestContainers {

    @Autowired
    private WebTestClient webTestClient;

    private String jwt;

    private WebTestClient.ResponseSpec getRequest(String path){
        return webTestClient
                .get()
                .uri(path)
                .header("Authorization", "Bearer " + jwt)
                .exchange();
    }

    private MarketDTO buildMarket(String name, LocalDate localDate){
        return MarketDTO.builder().name(name).startDate(localDate)
                .finishDate(localDate.plusDays(30))
                .build();
    }

    private MarketDTO createMarket(){
        MarketDTO buildMarket = buildMarket("Зимняя биржа 2024", LocalDate.now());
        MarketDTO market = webTestClient
                .post()
                .uri("/api/v1/market/create")
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(buildMarket), MarketDTO.class)
                .exchange()
                .expectBody(MarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(market);
        assertMarket(market, buildMarket);
        assertSame(market.getStatus(), MarketStatus.NEW);
        return market;
    }

    private List<MarketDTO> getActiveMarkets(){
        List<MarketDTO> market = getRequest("/api/v1/market/active")
                .expectBodyList(MarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(market);
        assertSame(market.get(0).getStatus(), MarketStatus.ACTIVE);
        return market;
    }

    private MarketDTO updateStatus(String marketId){
        MarketDTO marketDTO = webTestClient
                .put()
                .uri("/api/v1/market/status/{marketId}/{status}", marketId, MarketStatus.ACTIVE)
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(MarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(marketDTO);
        return marketDTO;
    }

    private void assertMarket(MarketDTO market, MarketDTO buildMarket){
        assertNotNull(market.getId());
        assertEquals(buildMarket.getName(), market.getName());
        assertEquals(buildMarket.getStartDate(), market.getStartDate());
        assertEquals(buildMarket.getFinishDate(), market.getFinishDate());
    }

    @BeforeAll
    public void setUp() {
        RegisterRequest request = new RegisterRequest(
                "market.test@gmail.com", "market", "test", "market-test",
                List.of(Role.ADMIN,
                        Role.EXPERT,
                        Role.PROJECT_OFFICE,
                        Role.INITIATOR,
                        Role.TEAM_OWNER));

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

    ///////////////////////
    //  _____   ____ ______
    // / ___/  / __//_  __/
    /// (_ /  / _/   / /
    //\___/  /___/  /_/
    ///////////////////////

    @Test
    void testGetAll() {
        createMarket();
        createMarket();
        List<MarketDTO> markets = getRequest("/api/v1/market/all")
                .expectBodyList(MarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(markets);
        assertTrue(markets.size() >= 2);
    }

    @Test
    void testGetActiveMarkets() {
        createMarket();
        updateStatus(createMarket().getId());
        updateStatus(createMarket().getId());
        getActiveMarkets();
    }

    @Test
    void testGetMarket() {
        MarketDTO marketDTO = createMarket();
        MarketDTO market = webTestClient
                .get()
                .uri("/api/v1/market/{marketId}", marketDTO.getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(MarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(market);
        assertEquals(marketDTO.getName(), market.getName());
    }

    //////////////////////////////
    //   ___   ____    ____ ______
    //  / _ \ / __ \  / __//_  __/
    // / ___// /_/ / _\ \   / /
    ///_/    \____/ /___/  /_/
    //////////////////////////////

    @Test
    void testCreateMarket(){
        createMarket();
    }

    ///////////////////////////////////////////
    //   ___    ____   __    ____ ______   ____
    //  / _ \  / __/  / /   / __//_  __/  / __/
    // / // / / _/   / /__ / _/   / /    / _/
    ///____/ /___/  /____//___/  /_/    /___/
    ///////////////////////////////////////////

    @Test
    void testDeleteMarket(){
        InfoResponse infoResponse = webTestClient
                .delete()
                .uri("/api/v1/market/delete/{marketId}", createMarket().getId())
                .header("Authorization", "Bearer " + jwt)
                .exchange()
                .expectBody(InfoResponse.class)
                .returnResult().getResponseBody();
        assertNotNull(infoResponse);
        assertEquals(infoResponse.getMessage(), "Успешное удаление");
    }

    ////////////////////////
    //   ___   __  __ ______
    //  / _ \ / / / //_  __/
    // / ___// /_/ /  / /
    ///_/    \____/  /_/
    ////////////////////////

    @Test
    void testUpdateMarket(){
        createMarket();
        MarketDTO buildMarket = buildMarket("Весенняя биржа 2024", LocalDate.now());
        MarketDTO market = webTestClient
                .put()
                .uri("/api/v1/market/update/{marketId}", createMarket().getId())
                .header("Authorization", "Bearer " + jwt)
                .body(Mono.just(buildMarket), MarketDTO.class)
                .exchange()
                .expectBody(MarketDTO.class)
                .returnResult().getResponseBody();
        assertNotNull(market);
        assertMarket(market, buildMarket);
        assertSame(market.getStatus(), MarketStatus.NEW);
    }

    @Test
    void testUpdateStatus(){
        MarketDTO market1 = updateStatus(createMarket().getId());
        assertMarket(market1, market1);
        assertSame(market1.getStatus(), MarketStatus.ACTIVE);
        String marketId = createMarket().getId();
        MarketDTO market2 = updateStatus(marketId);
        assertMarket(market2, market2);
        assertSame(market2.getStatus(), MarketStatus.ACTIVE);
        assertEquals(getActiveMarkets().get(1).getId(), market2.getId());
    }
}
