package com.tyiu.corn.service;

import lombok.RequiredArgsConstructor;

import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@EnableScheduling
public class AccountChangeService {}
//    @Autowired
//    private final JavaMailSender emailSender;
//
//    private final AccountChangeRepository accountChangeRepository;
//
//    private final UserRepository userRepository;
//
//    private final PasswordEncoder passwordEncoder;
//
//    private void sendEmail(String toAdresses, String subject, String message){
//        SimpleMailMessage simpleMailMessage = new SimpleMailMessage();
//        simpleMailMessage.setTo(toAdresses);
//        simpleMailMessage.setSubject(subject);
//        simpleMailMessage.setText(message);
//        this.emailSender.send(simpleMailMessage);
//    }

    // private boolean containsRole(final List<Role> list, final Role string){
    //     return list.stream().filter(listEmail -> listEmail == string).findFirst().isPresent();
    // }

//    public void sendInvitations(InvitationDTO invitations) throws MailSendException, NotFoundException, ParseException {
//        if (invitations.getRoles() == null){
//            throw new NotFoundException("Добавьте роли");
//        }
//        try {
//            invitations.getEmails().stream().filter(email -> !userRepository.existsByEmail(email)).forEach((email) ->
//                {
//                    Date date = new Date();
//                    long milliseconds = date.getTime() + 259200000;
//                    date.setTime(milliseconds);
//                    Temporary invitation = new Temporary();
//                    invitation.setRoles(invitations.getRoles());
//                    invitation.setEmail(email);
//                    invitation.setDateExpired(date);
//                    invitation.setUrl(UUID.randomUUID().toString());
//                    sendEmail(
//                        invitation.getEmail(),
//                        "Приглашение",
//                        String.format("Приглашение на регистрацию http://localhost:8080/register/%s", invitation.getUrl())
//                    );
//                    accountChangeRepository.save(invitation);
//                }
//            );
//        } catch ( MailSendException e){
//            throw new EmailSendException("В списке есть почта без домена");
//        } catch (MailParseException e){
//            throw new ParseException("В списке есть почта без имени пользователя");
//        } catch ( NullPointerException e){
//            throw new NotFoundException("Добавьте фаил с почтами");
//        }
//    }

//    public void sendInvitation(Temporary invitation) throws EmailSendException, NotFoundException, ParseException{
//        if (userRepository.existsByEmail(invitation.getEmail())){
//            throw new UserExistsException("Пользователь с такой почтой существует");
//        }
//        UUID url = UUID.randomUUID();
//        try {
//            Date date = new Date();
//            long milliseconds = date.getTime() + 259200000;
//            date.setTime(milliseconds);
//            invitation.setRoles(List.of(Role.INITIATOR));
//            invitation.setDateExpired(date);
//            invitation.setUrl(url.toString());
//            sendEmail(
//                invitation.getEmail(),
//                "Приглашение",
//                String.format("Приглашение на регистрацию http://localhost:8080/register/%s", invitation.getUrl())
//            );
//            if (accountChangeRepository.existsByEmail(invitation.getEmail())){
//            accountChangeRepository.deleteByEmail(invitation.getEmail());
//            }
//        } catch (MailSendException e) {
//            throw new EmailSendException("Добавьте домен почты");
//        } catch (NullPointerException e) {
//            throw new NotFoundException("Добавьте почту");
//        } catch (MailParseException e){
//            throw new ParseException("Добавьте имя пользователя почты");
//        }
//        accountChangeRepository.save(invitation);
//    }

//    public InvitationResponse findByUrl(String url) {
//        Temporary invitation = accountChangeRepository.findByUrl(url).orElseThrow(
//            () -> new NotFoundException("Приглашения " + url + " не существует"));
//        return InvitationResponse.builder()
//                .email(invitation.getEmail())
//                .roles(invitation.getRoles())
//                .build();
//    }

//    public ChangeResponse findByUrlAndSendCode(String url) {
//        Temporary emailChange = accountChangeRepository.findByUrl(url).orElseThrow(
//            () -> new NotFoundException("Доступ зарпрещен"));
//        if (userRepository.existsByEmail(emailChange.getNewEmail())){
//            throw new UserExistsException("Пользователь с такой почтой существует");
//        }
//        sendEmail(
//            emailChange.getOldEmail(),
//            "Код для изменения почты",
//            String.format("Введите этот код для изменения почты %d", emailChange.getCode())
//        );
//        return ChangeResponse.builder()
//            .newEmail(emailChange.getNewEmail())
//            .oldEmail(emailChange.getOldEmail())
//            .build();
//    }

//    public void sendEmailToChangeEmail(Temporary emailChange){
//        if (userRepository.existsByEmail(emailChange.getNewEmail())){
//            throw new UserExistsException("Пользователь с такой почтой существует");
//        }
//        if (accountChangeRepository.existsByOldEmail(emailChange.getOldEmail())){
//            accountChangeRepository.deleteByOldEmail(emailChange.getOldEmail());
//        }
//        try{
//            Date date = new Date();
//            date.setTime(date.getTime()+43200000);
//            emailChange.setUrl(UUID.randomUUID().toString());
//            emailChange.setDateExpired(date);
//            emailChange.setCode(new Random(System.currentTimeMillis()).nextInt(900000)+100000);
//            sendEmail(
//                emailChange.getNewEmail(),
//                "Изменение почты",
//                String.format("Ссылка для смены почты: http://localhost:8080/change-email/%s", emailChange.getUrl())
//            );
//            accountChangeRepository.save(emailChange);
//        }catch (MailSendException e) {
//            throw new EmailSendException("Добавьте домен почты");
//        } catch (NullPointerException e) {
//            throw new NotFoundException("Добавьте почту");
//        } catch (MailParseException e){
//            throw new ParseException("Добавьте имя пользователя почты");
//        }
//    }

//    public String sendEmailToChangePassword(Temporary passwordChange) {
//        if (!userRepository.existsByEmail(passwordChange.getEmail())) {
//            throw new NotFoundException(
//                    String.format("Пользователя с почтой %s не существует", passwordChange.getEmail()));
//        }
//        Date date = new Date();
//        date.setTime(date.getTime() + 300000);
//        passwordChange.setDateExpired(date);
//        passwordChange.setUrl(UUID.randomUUID().toString());
//        passwordChange.setCode(new Random(System.currentTimeMillis()).nextInt(900000) + 100000);
//        sendEmail(
//                passwordChange.getEmail(),
//                "Восстановление пароля",
//                String.format("Введите этот код для восстановления пароля: %d", passwordChange.getCode()));
//        accountChangeRepository.save(passwordChange);
//        return passwordChange.getUrl();
//    }

//    public void changePasswordByUser(ChangeRequest request){
//        Temporary changePassword = accountChangeRepository.findByUrl(request.getKey()).orElseThrow(
//            () -> new NotFoundException("Доступ зарпрещен")
//        );
//        if (new Date().getTime()>changePassword.getDateExpired().getTime()){
//            accountChangeRepository.deleteByUrl(changePassword.getUrl());
//            throw new DateExpiredException("Время действия кода истекло");
//
//        }
//        if (request.getCode() == changePassword.getCode()) {
//            User user = userRepository.findByEmail(changePassword.getEmail()).get();
//            userRepository.setPassword(passwordEncoder.encode(request.getPassword()), user.getId());
//            accountChangeRepository.delete(changePassword);
//        } else {
//            throw new AuthorizationNotSuccessException("Неправильный код");
//        }
//    }


//    public void changeEmailByUser(ChangeRequest request){
//        Temporary emailChange = accountChangeRepository.findByUrl(request.getUrl()).orElseThrow(
//            () -> new NotFoundException("Доступ зарпрещен")
//        );
//        if (emailChange.getCode() == request.getCode()){
//            User user = userRepository.findByEmail(request.getOldEmail()).get();
//            userRepository.setEmail(request.getNewEmail(), user.getId());
//            accountChangeRepository.delete(emailChange);
//        } else {
//            throw new AuthorizationNotSuccessException("Неправильный код");
//        }
//    }

//    public List<UserInfoResponse> getUsersInfo(){
//        List<UserInfoResponse> usersInfo = new ArrayList<UserInfoResponse>();
//        userRepository.findAll().stream().forEach(
//            user -> {
//                usersInfo.add(UserInfoResponse.builder()
//                                .email(user.getEmail())
//                                .roles(user.getRoles())
//                                .firstName(user.getFirstName())
//                                .lastName(user.getLastName())
//                                .build());
//            }
//        );
//        return usersInfo;
//    }

//    public List<String> getAllEmails(){
//        List<String> usersEmails = new ArrayList<String>();
//        userRepository.findAll().stream().forEach(
//            user -> {
//                usersEmails.add(user.getEmail());
//            }
//        );
//        return usersEmails;
//    }

//    public void changeUserInfo(UserInfoRequest request){
//        Mono<User> user = userRepository.findByEmail(request.getEmail());
//        userRepository.setUserInfo(
//            request.getNewEmail(),
//            request.getNewFirstName(),
//            request.getNewLastName(),
//            request.getNewRoles(),
//            user.getId()
//        );
//    }

//    public void deleteDataByUrl(String url){
//        accountChangeRepository.deleteByUrl(url);
//    }

//    @Scheduled(fixedRate = 43200000)
//    public void deleteExpiredData(){
//        Date date = new Date();
//        accountChangeRepository.deleteExpiredData(date);
//    }
//}
