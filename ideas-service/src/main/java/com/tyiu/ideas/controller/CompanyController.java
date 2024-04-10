package com.tyiu.ideas.controller;

import com.tyiu.ideas.config.exception.NotFoundException;
import com.tyiu.ideas.model.dto.CompanyDTO;
import com.tyiu.ideas.model.dto.UserDTO;
import com.tyiu.ideas.model.responses.InfoResponse;
import com.tyiu.ideas.service.CompanyService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@RestController
@RequestMapping("/api/v1/ideas-service/company")
@RequiredArgsConstructor
public class CompanyController {

    private final CompanyService companyService;

    @GetMapping("/{companyId}")
    @PreAuthorize("hasAnyRole('INITIATOR','ADMIN')")
    public Mono<CompanyDTO> getCompanyById(@PathVariable String companyId) {
        return companyService.getCompanyById(companyId)
                .switchIfEmpty(Mono.error(new NotFoundException("Компания не найдена")));
    }

    @GetMapping("/owner")
    @PreAuthorize("hasAnyRole('INITIATOR','ADMIN')")
    public Flux<CompanyDTO> getMemberListCompany(@AuthenticationPrincipal Jwt jwt) {
        return companyService.getMembersListCompany(jwt.getId());
    }

    @GetMapping("/all")
    @PreAuthorize("hasAnyRole('INITIATOR','ADMIN')")
    public Flux<CompanyDTO> getCompanyList() {
        return companyService.getListCompany();
    }

    @GetMapping("/staff/{companyId}")
    @PreAuthorize("hasAnyRole('INITIATOR','ADMIN')")
    public Flux<UserDTO> getCompanyStaff(@PathVariable String companyId) {
        return companyService.getListStaff(companyId);
    }

    @PostMapping("/create")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<CompanyDTO> createCompany(@RequestBody CompanyDTO company) {
        return companyService.createCompany(company)
                .switchIfEmpty(Mono.error(new NotFoundException("Не удалось создать компанию")));
    }

    @DeleteMapping("/delete/{companyId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<InfoResponse> deleteCompany(@PathVariable String companyId) {
        return companyService.deleteCompany(companyId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Компания успешно удалена"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Ошибка при удалении компании"));
    }

    @PutMapping("/update/{companyId}")
    @PreAuthorize("hasRole('ADMIN')")
    public Mono<CompanyDTO> updateCompany(@PathVariable String companyId,
                                          @RequestBody CompanyDTO company) {
        return companyService.updateCompany(companyId, company)
                .switchIfEmpty(Mono.error(new NotFoundException("Ошибки при обновлении компании")));
    }
}
