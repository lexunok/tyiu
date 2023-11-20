package com.tyiu.corn.controller;

import com.tyiu.corn.config.exception.NotFoundException;
import com.tyiu.corn.model.dto.CompanyDTO;
import com.tyiu.corn.model.dto.UserDTO;
import com.tyiu.corn.model.entities.User;
import com.tyiu.corn.model.responses.InfoResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import com.tyiu.corn.service.CompanyService;
import lombok.RequiredArgsConstructor;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@RestController
@RequestMapping("/api/v1/company")
@RequiredArgsConstructor
public class CompanyController {

    private final CompanyService companyService;

    @GetMapping("/{companyId}")
    public Mono<CompanyDTO> getCompanyById(@PathVariable String companyId) {
        return companyService.getCompanyById(companyId)
                .switchIfEmpty(Mono.error(new NotFoundException("Not found!")));
    }

    @GetMapping("/owner")
    public Flux<CompanyDTO> getMemberListCompany(@AuthenticationPrincipal User user) {
        return companyService.getMembersListCompany(user.getId());
    }

    @GetMapping("/all")
    public Flux<CompanyDTO> getCompanyList() {
        return companyService.getListCompany();
    }

    @GetMapping("/staff/{companyId}")
    public Flux<UserDTO> getCompanyStaff(@PathVariable String companyId) {
        return companyService.getListStaff(companyId);
    }

    @PostMapping("/create")
    public Mono<CompanyDTO> createCompany(@RequestBody CompanyDTO company) {
        return companyService.createCompany(company)
                .switchIfEmpty(Mono.error(new NotFoundException("Create is not success!")));
    }

    @DeleteMapping("/delete/{companyId}")
    public Mono<InfoResponse> deleteCompany(@PathVariable String companyId) {
        return companyService.deleteCompany(companyId)
                .thenReturn(new InfoResponse(HttpStatus.OK,"Success deleting"))
                .onErrorReturn(new InfoResponse(HttpStatus.BAD_REQUEST,"Delete is not success!"));
    }

    @PutMapping("/update/{companyId}")
    public Mono<CompanyDTO> updateCompany(@PathVariable String companyId,
                                            @RequestBody CompanyDTO company) {
        return companyService.updateCompany(companyId, company)
                .switchIfEmpty(Mono.error(new NotFoundException("Update is not success")));
    }
}
