package com.tyiu.corn.controller;

import com.tyiu.corn.model.dto.CompanyDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
    public Mono<CompanyDTO> getCompanyById(@PathVariable Long companyId) {
        return companyService.getCompanyById(companyId);
    }

    @GetMapping("/all")
    public Flux<CompanyDTO> getCompanyList() {
        return companyService.getListCompany();
    }

    @GetMapping("/staff/{companyId}")
    public Mono<CompanyDTO> getCompanyStaff(@PathVariable Long companyId) {
        return companyService.getListStaff(companyId);
    }

    @PostMapping("/create")
    public Mono<CompanyDTO> createCompany(@RequestBody CompanyDTO company) {
        return companyService.createCompany(company);
    }

    @DeleteMapping("/delete/{companyId}")
    public Mono<ResponseEntity<String>> deleteCompany(@PathVariable Long companyId) {
        companyService.deleteCompany(companyId).subscribe();
        return Mono.just(new ResponseEntity<>("Success deleting", HttpStatus.OK));
    }

    @PutMapping("/update/{companyId}")
    public Mono<CompanyDTO> updateCompany(@PathVariable Long companyId, @RequestBody CompanyDTO company) {
        return companyService.updateCompany(companyId, company);
    }
}
