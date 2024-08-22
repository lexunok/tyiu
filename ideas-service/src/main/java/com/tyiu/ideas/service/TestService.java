package com.tyiu.ideas.service;

import com.tyiu.client.exceptions.ServerProcessException;
import com.tyiu.client.models.UserDTO;
import com.tyiu.ideas.model.dto.TestAnswerDTO;
import com.tyiu.ideas.model.dto.TestDTO;
import com.tyiu.ideas.model.dto.TestQuestionDTO;
import com.tyiu.ideas.model.dto.TestResultDTO;
import com.tyiu.ideas.model.entities.Test;
import com.tyiu.ideas.model.entities.TestQuestion;
import com.tyiu.ideas.model.entities.TestResult;
import com.tyiu.ideas.model.responses.TestAllResponse;
import io.r2dbc.spi.Batch;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.InputStreamResource;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import static org.springframework.data.relational.core.query.Criteria.where;
import static org.springframework.data.relational.core.query.Query.query;

@Service
@RequiredArgsConstructor
@EnableScheduling
public class TestService {

    private final R2dbcEntityTemplate template;

    private final String belbinTest = "BelbinTest";
    private final String temperTest = "TemperTest";
    private final String mindTest = "MindTest";

    private int number;
    private Integer module;

    private Mono<Void> belbinTestQuestionBuilder(String question){
        String moduleName = null;
        if (number > 7) { number = 0; module += 10; }
        switch (module) {
            case 10 -> moduleName = "БЛОК 1. ЧТО  Я  МОГУ  ПРЕДЛОЖИТЬ  КОМАНДЕ";
            case 20 -> moduleName = "БЛОК 2. ЧТО ХАРАКТЕРИЗУЕТ МЕНЯ КАК ЧЛЕНА КОМАНДЫ";
            case 30 -> moduleName = "БЛОК 3. КОГДА  Я   РАБОТАЮ  С  ДРУГИМИ  НАД  ПРОЕКТОМ";
            case 40 -> moduleName = "БЛОК 4. МОЕ ОТНОШЕНИЕ И ИНТЕРЕС К ГРУППОВОЙ РАБОТЕ";
            case 50 -> moduleName = "БЛОК 5. Я  ЧУВСТВУЮ УДОВЛЕТВОРЕНИЕ ОТ РАБОТЫ, ПОТОМУ ЧТО";
            case 60 -> moduleName = "БЛОК 6. КОГДА  ЗАДАНИЕ  ТРУДНОЕ  И НЕЗНАКОМОЕ";
            case 70 -> moduleName = "БЛОК 7. ПРОБЛЕМЫ, ВОЗНИКАЮЩИЕ ПРИ РАБОТЕ В ГРУППАХ";
        }
        return template.insert(TestQuestion.builder()
                        .testName(belbinTest)
                        .questionNumber(module + number)
                        .questionName(belbinTest + "QuestionModule" + (module / 10) + "Number" + number++)
                        .questionModuleNumber(module / 10)
                        .questionModule(moduleName)
                        .question(question)
                        .build())
                .then();
    }

    private Mono<Void> checkBelbinTestQuestion(){
        return template.count(query(where("test_name").is(belbinTest)), TestQuestion.class)
                .flatMap(c -> {
                    if (c != 56){
                        number = 0;
                        module = 10;
                        return template.delete(query(where("test_name").is(belbinTest)), TestQuestion.class)
                                //БЛОК 1
                                .then(belbinTestQuestionBuilder("Я думаю, что я в состоянии быстро воспринимать и использовать новые возможности"))
                                .then(belbinTestQuestionBuilder("Я легко кооперируюсь с людьми разных типов"))
                                .then(belbinTestQuestionBuilder("Один из моих главных активов – продуцировать новые идеи"))
                                .then(belbinTestQuestionBuilder("Я способен вовлекать людей, которые, по моему мнению, могут сделать большой вклад в достижение групповых целей"))
                                .then(belbinTestQuestionBuilder("Мои личные способности позволяют эффективно доводить дело до самого конца"))
                                .then(belbinTestQuestionBuilder("Я не представляю себе даже временного снижения своей популярности, даже если это приведет к увеличению прибыли"))
                                .then(belbinTestQuestionBuilder("Обычно я чувствую, что реалистично и что дееспособно"))
                                .then(belbinTestQuestionBuilder("Я способен предложить весомые аргументы в пользу другой линии действий, не провоцируя при этом предубеждений и предвзятости"))
                                //БЛОК 2
                                .then(belbinTestQuestionBuilder("Я чувствую себя неуютно на собраниях, даже если они четко структурированы и продуманно организованы"))
                                .then(belbinTestQuestionBuilder("Я склонен полагаться на людей, которые хорошо аргументируют свою точку зрения еще до того, как она была всесторонне обсуждена"))
                                .then(belbinTestQuestionBuilder("Когда группа обсуждает новые идеи, я склонен слишком много говорить"))
                                .then(belbinTestQuestionBuilder("Мои личные отношения мешают мне поддерживать коллег с энтузиазмом"))
                                .then(belbinTestQuestionBuilder("Когда надо сделать какое-либо дело, некоторые люди считают, что я действую агрессивно и авторитарно"))
                                .then(belbinTestQuestionBuilder("Я затрудняюсь брать на себя лидерскую роль, может потому, что слишком чувствителен к чувствам и настроениям группы"))
                                .then(belbinTestQuestionBuilder("У меня есть склонность настолько увлекаться собственными идеями, что я забываю о том, что происходит вокруг"))
                                .then(belbinTestQuestionBuilder("Мои коллеги считают, что я слишком забочусь о незначительных деталях и боюсь риска, что дело может быть испорчено"))
                                //БЛОК 3
                                .then(belbinTestQuestionBuilder("Я могу хорошо влиять на других людей, при этом, не оказывая на них сильного давления"))
                                .then(belbinTestQuestionBuilder("Мое «шестое чувство» подсказывает  и  предохраняет меня от ошибок и инцидентов, которые иногда случаются из-за небрежности"))
                                .then(belbinTestQuestionBuilder("Во имя достижения главных целей, я готов ускорять события, не тратя время на обсуждения"))
                                .then(belbinTestQuestionBuilder("От меня всегда можно ожидать чего-либо оригинального"))
                                .then(belbinTestQuestionBuilder("Я всегда готов поддержать хорошее предложение, которое принесет выгоду всем"))
                                .then(belbinTestQuestionBuilder("Я постоянно отслеживаю  последние идеи и новейшие достижения"))
                                .then(belbinTestQuestionBuilder("Я думаю, что мои способности к  суждениям и оценкам могут внести большой вклад в принятие правильных решений"))
                                .then(belbinTestQuestionBuilder("На меня всегда можно положиться на завершающем этапе работы"))
                                //БЛОК 4
                                .then(belbinTestQuestionBuilder("Я искренне желаю узнать моих коллег получше"))
                                .then(belbinTestQuestionBuilder("Я не боюсь ни оспаривать точку зрения другого человека, ни остаться в меньшинстве"))
                                .then(belbinTestQuestionBuilder("Обычно я могу доказать несостоятельность неудачного предложения"))
                                .then(belbinTestQuestionBuilder("Я думаю, что я способен хорошо выполнить любую функцию ради выполнения общего плана"))
                                .then(belbinTestQuestionBuilder("Часто я избегаю очевидных решений и прихожу вместо этого к неожиданным решениям проблемы"))
                                .then(belbinTestQuestionBuilder("Я стремлюсь все что я делаю доводить до совершенства"))
                                .then(belbinTestQuestionBuilder("Я готов использовать контакты вне группы"))
                                .then(belbinTestQuestionBuilder("Хотя я всегда открыт различным точкам зрения, я не испытываю трудностей при принятии решений"))
                                //БЛОК 5
                                .then(belbinTestQuestionBuilder("Мне нравится анализировать ситуации и оценивать возможные направления деятельности"))
                                .then(belbinTestQuestionBuilder("Мне интересно находить практические пути решения проблемы"))
                                .then(belbinTestQuestionBuilder("Мне приятно чувствовать, что я помогаю созданию хороших отношений на работе"))
                                .then(belbinTestQuestionBuilder("Часто я имею сильное влияние на принимаемые решения"))
                                .then(belbinTestQuestionBuilder("Я имею открытые, приветливые отношения с людьми, которые могут предложить что-то новенькое"))
                                .then(belbinTestQuestionBuilder("Я могу убеждать людей в необходимости определенной линии действий"))
                                .then(belbinTestQuestionBuilder("Я чувствую себя хорошо дома, когда я могу уделить максимум внимания заданию"))
                                .then(belbinTestQuestionBuilder("Я люблю работать с чем-либо, что стимулирует мое воображение"))
                                //БЛОК 6
                                .then(belbinTestQuestionBuilder("Я откладываю дело на время и размышляю над проблемой"))
                                .then(belbinTestQuestionBuilder("Я готов сотрудничать с людьми, которые более позитивно и с большим  энтузиазмом относятся к проблеме"))
                                .then(belbinTestQuestionBuilder("Я пытаюсь сделать задание проще, подыскивая в группе людей, которые могут взять на себя решение части проблемы"))
                                .then(belbinTestQuestionBuilder("Мое врожденное ощущение времени позволяет мне выдерживать сроки выполнения задания"))
                                .then(belbinTestQuestionBuilder("Я думаю, мне удастся сохранить ясность мысли и спокойствие"))
                                .then(belbinTestQuestionBuilder("Даже под давлением внешних обстоятельств я не отступаю от цели"))
                                .then(belbinTestQuestionBuilder("Я готов взять лидерские обязанности на себя, если я чувствую, что группа не прогрессирует"))
                                .then(belbinTestQuestionBuilder("Я бы начал дискуссию с целью стимулировать появление новых мыслей, способствующих решению проблемы"))
                                //БЛОК 7
                                .then(belbinTestQuestionBuilder("Я склонен выражать свое нетерпение по отношению к людям, которые стоят на пути развития прогресса (мешают)"))
                                .then(belbinTestQuestionBuilder("Другие могут критиковать меня за то, что я слишком аналитичен и не подключаю интуицию"))
                                .then(belbinTestQuestionBuilder("Мое желание убедиться в том, что работа выполняется с высоким качеством, может иногда привести к задержке"))
                                .then(belbinTestQuestionBuilder("Мне быстро все надоедает, и я полагаюсь на то, что кто-то из группы стимулирует мой интерес"))
                                .then(belbinTestQuestionBuilder("Мне трудно приступить к решению задачи, не имея четкой цели"))
                                .then(belbinTestQuestionBuilder("Иногда мне трудно объяснить и описать проблему в комплексе"))
                                .then(belbinTestQuestionBuilder("Я знаю, что я требую от других того, что я сам не могу выполнить"))
                                .then(belbinTestQuestionBuilder("Я затрудняюсь выражать собственное мнение, когда я нахожусь в очевидной оппозиции к большинству"))
                                .then();
                    }
                    return Mono.empty();
                }).then();
    }
    
    private Mono<Void> temperTestQuestionBuilder(String question){
        return template.insert(TestQuestion.builder()
                        .testName(temperTest)
                        .questionNumber(number)
                        .questionName(temperTest + "QuestionNumber" + number++)
                        .questionModuleNumber(0)
                        .questionModule("-")
                        .question(question)
                        .build())
                .then();
    }

    private Mono<Void> checkTemperTestQuestion(){
        return template.count(query(where("test_name").is(temperTest)), TestQuestion.class)
                .flatMap(c -> {
                    if (c != 57){
                        number = 1;
                        return template.delete(query(where("test_name").is(temperTest)), TestQuestion.class)
                                .then(temperTestQuestionBuilder("Часто ли вы испытываете тягу к новым впечатлениям, к тому, чтобы отвлечься, испытывать сильные ощущения?"))
                                .then(temperTestQuestionBuilder("Часто ли вы чувствуете, что нуждаетесь в друзьях, которые могут вас понять, ободрить или посочувствовать?"))
                                .then(temperTestQuestionBuilder("Считаете ли вы себя беззаботным человеком?"))
                                .then(temperTestQuestionBuilder("Очень ли трудно вам отказываться от своих намерений?"))
                                .then(temperTestQuestionBuilder("Обдумываете ли вы свои дела не спеша и предпочитаете ли подождать, прежде чем действовать?"))
                                .then(temperTestQuestionBuilder("Всегда ли вы сдерживаете свои обещания, даже если это вам невыгодно?"))
                                .then(temperTestQuestionBuilder("Часто ли у вас бывают спады и подъемы настроения?"))
                                .then(temperTestQuestionBuilder("Быстро ли вы обычно действуете и говорите, не тратите ли много времени на обдумывание?"))
                                .then(temperTestQuestionBuilder("Возникало ли у вас когда-нибудь чувство, что вы несчастны, хотя никакой серьезной причины для этого не было?"))
                                .then(temperTestQuestionBuilder("Верно ли, что на спор вы способны решиться на все?"))
                                //10
                                .then(temperTestQuestionBuilder("Смущаетесь ли вы, когда хотите познакомиться с человеком противоположного пола, который вам симпатичен?"))
                                .then(temperTestQuestionBuilder("Бывает ли когда-нибудь, что, разозлившись, вы выходите из себя?"))
                                .then(temperTestQuestionBuilder("Часто ли бывает, что вы действуете необдуманно, под влиянием момента?"))
                                .then(temperTestQuestionBuilder("Часто ли вас беспокоят мысли о том, что вам не следовало чего-либо делать или говорить?"))
                                .then(temperTestQuestionBuilder("Предпочитаете ли вы чтение книг встречам с людьми?"))
                                .then(temperTestQuestionBuilder("Верно ли, что вас легко задеть?"))
                                .then(temperTestQuestionBuilder("Любите ли вы часто бывать в компании?"))
                                .then(temperTestQuestionBuilder("Бывают ли иногда у вас такие мысли, которыми вам не хотелось бы делиться с другими людьми?"))
                                .then(temperTestQuestionBuilder("Верно ли, что иногда вы настолько полны энергии, что все горит в руках, а иногда чувствуете сильную вялость?"))
                                .then(temperTestQuestionBuilder("Стараетесь ли вы ограничить круг своих знакомых небольшим числом самых близких друзей?"))
                                //20
                                .then(temperTestQuestionBuilder("Много ли вы мечтаете?"))
                                .then(temperTestQuestionBuilder("Когда на вас кричат, отвечаете ли вы тем же?"))
                                .then(temperTestQuestionBuilder("Считаете ли вы все свои привычки хорошими?"))
                                .then(temperTestQuestionBuilder("Часто ли у вас появляется чувство, что вы в чем-то виноваты?"))
                                .then(temperTestQuestionBuilder("Способны ли вы иногда дать волю своим чувствам и беззаботно развлечься с веселой компанией?"))
                                .then(temperTestQuestionBuilder("Можно ли сказать, что нервы у вас часто бывают натянуты до предела?"))
                                .then(temperTestQuestionBuilder("Слывете ли вы за человека живого и веселого?"))
                                .then(temperTestQuestionBuilder("После того как дело сделано, часто ли вы мысленно возвращаетесь к нему и думаете, что могли бы сделать лучше?"))
                                .then(temperTestQuestionBuilder("Чувствуете ли вы себя неспокойно, находясь в большой компании?"))
                                .then(temperTestQuestionBuilder("Бывает ли, что вы передаете слухи?"))
                                //30
                                .then(temperTestQuestionBuilder("Бывает ли, что вам не спится из-за того, что в голову лезут разные мысли?"))
                                .then(temperTestQuestionBuilder("Что вы предпочитаете, если хотите узнать что-либо: найти в книге или спросить у друзей?"))
                                .then(temperTestQuestionBuilder("Бывают ли у вас сильные сердцебиения?"))
                                .then(temperTestQuestionBuilder("Нравится ли вам работа, требующая сосредоточения?"))
                                .then(temperTestQuestionBuilder("Бывают ли у вас приступы дрожи?"))
                                .then(temperTestQuestionBuilder("Всегда ли вы говорите только правду?"))
                                .then(temperTestQuestionBuilder("Бывает ли вам неприятно находиться в компании, где все подшучивают друг над другом?"))
                                .then(temperTestQuestionBuilder("Раздражительны ли вы?"))
                                .then(temperTestQuestionBuilder("Нравится ли вам работа, требующая быстрого действия?"))
                                .then(temperTestQuestionBuilder("Верно ли, что вам часто не дают покоя мысли о разных неприятностях и «ужасах», которые могли бы произойти, хотя все кончилось благополучно?"))
                                //40
                                .then(temperTestQuestionBuilder("Верно ли, что вы неторопливы в движениях и несколько медлительны?"))
                                .then(temperTestQuestionBuilder("Опаздывали ли вы когда-нибудь на работу или на встречу с кем-либо?"))
                                .then(temperTestQuestionBuilder("Часто ли вам снятся кошмары?"))
                                .then(temperTestQuestionBuilder("Верно ли, что вы так любите поговорить, что не упускаете любого удобного случая побеседовать с новым человеком?"))
                                .then(temperTestQuestionBuilder("Беспокоят ли вас какие-нибудь боли?"))
                                .then(temperTestQuestionBuilder("Огорчились бы вы, если бы долго не могли видеться со своими друзьями?"))
                                .then(temperTestQuestionBuilder("Можете ли вы назвать себя нервным человеком?"))
                                .then(temperTestQuestionBuilder("Есть ли среди ваших знакомых такие, которые вам явно не нравятся?"))
                                .then(temperTestQuestionBuilder("Могли бы вы сказать, что вы уверенный в себе человек?"))
                                .then(temperTestQuestionBuilder("Легко ли вас задевает критика ваших недостатков или вашей работы?"))
                                //50
                                .then(temperTestQuestionBuilder("Трудно ли вам получить настоящее удовольствие от мероприятий, в которых участвует много народа?"))
                                .then(temperTestQuestionBuilder("Беспокоит ли вас чувство, что вы чем-то хуже других?"))
                                .then(temperTestQuestionBuilder("Сумели бы вы внести оживление в скучную компанию?"))
                                .then(temperTestQuestionBuilder("Бывает ли, что вы говорите о вещах, в которых совсем не разбираетесь?"))
                                .then(temperTestQuestionBuilder("Беспокоитесь ли вы о своем здоровье?"))
                                .then(temperTestQuestionBuilder("Любите ли вы подшутить над другими?"))
                                .then(temperTestQuestionBuilder("Страдаете ли вы бессонницей?"))
                                .then();
                    }
                    return Mono.empty();
                });
    }

    private Mono<Void> mindTestQuestionBuilder(String question){
        String moduleName = null;
        if (number > 5) { number = 1; ++module; }
        switch (module){
            case 1 -> moduleName = "Когда между людьми имеет место конфликт на почве идей, я отдаю предпочтение той стороне, которая:";
            case 2 -> moduleName = "Когда я начинаю решать какие-либо проблемы совместно с другими людьми, самое важное для меня:";
            case 3 -> moduleName = "Вообще говоря, я усваиваю новые идеи лучше всего, когда могу:";
            case 4 -> moduleName = "Для меня графики, схемы, чертежи в книгах или статьях обычно:";
            case 5 -> moduleName = "Если бы мне предложили провести какое-то исследование (например, курсовую или дипломную работу), я, вероятно начал бы с:";
            case 6 -> moduleName = "Если бы мне пришлось собирать от членов какой-то организации информацию, касающуюся ее насущных проблем, я предпочел бы:";
            case 7 -> moduleName = "Вероятно, я буду считать что-то правильным, истинным, если это “что-то”:";
            case 8 -> moduleName = "Когда я на досуге читаю журнальную статью, она будет скорее всего:";
            case 9 -> moduleName = "Когда я читаю отчет о работе (или другой текст, например, научный или учебный), я обращаю больше всего внимания на:";
            case 10 -> moduleName = "Когда передо мной поставлена задача, первое, что я хочу узнать – это:";
            case 11 -> moduleName = "Обычно я узнаю максимум о том, как сделать что-то новое, благодаря тому, что:";
            case 12 -> moduleName = "Если бы мне пришлось проходить испытание или сдавать экзамен, я предпочел бы:";
            case 13 -> moduleName = "Люди, чьи особые качества я уважаю больше всего, это вероятно:";
            case 14 -> moduleName = "Вообще говоря, я нахожу теорию полезной, если она:";
            case 15 -> moduleName = "Когда я читаю статью по дискуссионному вопросу (или, например, наблюдаю дискуссию в телевизионной передаче), я предпочитаю, чтобы в ней:";
            case 16 -> moduleName = "Когда я читаю книгу, выходящую за рамки моей непосредственной деятельности (учебной, профессиональной и т.п.), я делаю это главным образом вследствие:";
            case 17 -> moduleName = "Когда я впервые подхожу к какой-то технической проблеме (например, устранить несложную поломку в электроприборе), я скорее всего буду:";
            case 18 -> moduleName = "Вообще говоря, я более всего склонен к тому, чтобы:";
        }
        return template.insert(TestQuestion.builder()
                        .testName(mindTest)
                        .questionNumber(number)
                        .questionName(mindTest + "QuestionModule" + module + "Number" + number++)
                        .questionModuleNumber(module)
                        .questionModule(moduleName)
                        .question(question)
                        .build())
                .then();
    }

    private Mono<Void> checkMindTestQuestion(){
        return template.count(query(where("test_name").is(mindTest)), TestQuestion.class)
                .flatMap(c -> {
                    if (c != 90){
                        number = 1;
                        module = 1;
                        return template.delete(query(where("test_name").is(mindTest)), TestQuestion.class)
                                //1
                                .then(mindTestQuestionBuilder("Устанавливает, определяет конфликт и пытается его выразить открыто"))
                                .then(mindTestQuestionBuilder("Лучше всех выражает затрагиваемые ценности и идеалы"))
                                .then(mindTestQuestionBuilder("Лучше всех отражает мои личные взгляды и опыт"))
                                .then(mindTestQuestionBuilder("Подходит к ситуации наиболее логично и последовательно"))
                                .then(mindTestQuestionBuilder("Излагает аргументы наиболее кратко и убедительно"))
                                //2
                                .then(mindTestQuestionBuilder("Понять цели и значение будущей работы"))
                                .then(mindTestQuestionBuilder("Раскрыть цели и ценности участников рабочей группы"))
                                .then(mindTestQuestionBuilder("Определить порядок конкретных шагов по решению проблемы"))
                                .then(mindTestQuestionBuilder("Понять, какую выгоду может принести эта работа для нашей группы"))
                                .then(mindTestQuestionBuilder("Чтобы работа над проблемой была организована и сдвинулась с места"))
                                //3
                                .then(mindTestQuestionBuilder("Связать их с текущими или будущими своими занятиями"))
                                .then(mindTestQuestionBuilder("Применить их к конкретным ситуациям"))
                                .then(mindTestQuestionBuilder("Сосредоточиться на них тщательно и проанализировать"))
                                .then(mindTestQuestionBuilder("Понять, насколько они сходны с привычными мне идеями"))
                                .then(mindTestQuestionBuilder("Противопоставить их другим идеям"))
                                //4
                                .then(mindTestQuestionBuilder("Полезнее текста, если они точны"))
                                .then(mindTestQuestionBuilder("Полезны, если они ясно показывают новые факты"))
                                .then(mindTestQuestionBuilder("Полезны, если они подкрепляются и поясняются текстом"))
                                .then(mindTestQuestionBuilder("Полезны, если они поднимают вопросы по тексту"))
                                .then(mindTestQuestionBuilder("Не более и не менее полезны, чем другие материалы"))
                                //5
                                .then(mindTestQuestionBuilder("Попытки определить его место в более широком контексте"))
                                .then(mindTestQuestionBuilder("Определения того, смогу ли я выполнить его в одиночку или мне потребуется помощь"))
                                .then(mindTestQuestionBuilder("Размышлений и предположений о возможных результатах"))
                                .then(mindTestQuestionBuilder("Решения о том, следует ли вообще проводить это исследование"))
                                .then(mindTestQuestionBuilder("Попытки сформулировать проблему как можно полнее и точнее"))
                                //6
                                .then(mindTestQuestionBuilder("Встретиться с ними индивидуально и задать каждому свои конкретные вопросы"))
                                .then(mindTestQuestionBuilder("Провести общее собрание и попросить их высказать свое мнение"))
                                .then(mindTestQuestionBuilder("Опросить их небольшими группами, задавая общие вопросы"))
                                .then(mindTestQuestionBuilder("Встретиться неофициально с влиятельными лицами и выяснить их взгляды"))
                                .then(mindTestQuestionBuilder("Попросить членов организации предоставить мне (желательно в письменно форме) всю относящуюся к делу информацию, которой они располагают"))
                                //7
                                .then(mindTestQuestionBuilder("Выстояло против оппозиции, выдержало сопротивление противоположных подходов"))
                                .then(mindTestQuestionBuilder("Согласуется с другими вещами, которым я верю"))
                                .then(mindTestQuestionBuilder("Было подтверждено на практике"))
                                .then(mindTestQuestionBuilder("Поддается логическому и научному доказательству"))
                                .then(mindTestQuestionBuilder("Можно проверить лично на доступных наблюдению фактах"))
                                //8
                                .then(mindTestQuestionBuilder("О том, как кому-нибудь удалось разрешить личную или социальную проблему"))
                                .then(mindTestQuestionBuilder("Посвящена дискуссионному политическому или социальному вопросу"))
                                .then(mindTestQuestionBuilder("Сообщением о научном или историческом исследовании"))
                                .then(mindTestQuestionBuilder("Об интересном, забавном человеке или событии"))
                                .then(mindTestQuestionBuilder("Точным, без доли вымысла, сообщением о чьем-то интересном жизненном опыте"))
                                //9
                                .then(mindTestQuestionBuilder("Близость выводов моему личному опыту"))
                                .then(mindTestQuestionBuilder("Возможность выполнения данных в тексте рекомендаций"))
                                .then(mindTestQuestionBuilder("Надежность и обоснованность результатов фактическими данными"))
                                .then(mindTestQuestionBuilder("Понимание автором целей и задач работы"))
                                .then(mindTestQuestionBuilder("Интерпретацию, объяснение данных"))
                                //10
                                .then(mindTestQuestionBuilder("Каков наилучший метод для решения данной задачи"))
                                .then(mindTestQuestionBuilder("Кому и когда нужно, чтобы эта задача была решена"))
                                .then(mindTestQuestionBuilder("Почему эту задачу стоит решать"))
                                .then(mindTestQuestionBuilder("Какое влияние ее решение может иметь на другие задачи, которые приходиться решать"))
                                .then(mindTestQuestionBuilder("Какова прямая, немедленная выгода от решения данной задачи"))
                                //11
                                .then(mindTestQuestionBuilder("Уясняю для себя, как это связано с чем-то что мне хорошо знакомо"))
                                .then(mindTestQuestionBuilder("Принимаюсь за дело как можно раньше"))
                                .then(mindTestQuestionBuilder("Выслушиваю различные точки зрения, по поводу того, как это сделать"))
                                .then(mindTestQuestionBuilder("Есть кто-то, кто показывает мне, как это сделать"))
                                .then(mindTestQuestionBuilder("Тщательно анализирую, как это сделать наилучшим образом"))
                                //12
                                .then(mindTestQuestionBuilder("Набор объективных, проблемно-ориентированых вопросов по предмету"))
                                .then(mindTestQuestionBuilder("Дискуссию с теми, кто также проходит испытание"))
                                .then(mindTestQuestionBuilder("Устное изложение и показ того, что я знаю"))
                                .then(mindTestQuestionBuilder("Сообщение в свободной форме о том, как я применил на деле то, чему научился"))
                                .then(mindTestQuestionBuilder("Письменный отчет, охватывающий историю вопроса, теорию и метод"))
                                //13
                                .then(mindTestQuestionBuilder("Выдающиеся философы и ученые"))
                                .then(mindTestQuestionBuilder("Писатели и учителя"))
                                .then(mindTestQuestionBuilder("Лидеры деловых и политических кругов"))
                                .then(mindTestQuestionBuilder("Экономисты и инженеры"))
                                .then(mindTestQuestionBuilder("Фермеры и журналисты"))
                                //14
                                .then(mindTestQuestionBuilder("Кажется родственной тем другим теориям и идеям, которые я уже усвоил"))
                                .then(mindTestQuestionBuilder("Объясняет вещи новым для меня образом"))
                                .then(mindTestQuestionBuilder("Способна систематически объяснять множество связанных ситуаций"))
                                .then(mindTestQuestionBuilder("Служит пояснению моего личного опыта и наблюдений"))
                                .then(mindTestQuestionBuilder("Имеет конкретное практическое приложение"))
                                //15
                                .then(mindTestQuestionBuilder("Показывались преимущества для меня в зависимости от выбираемой точки зрения"))
                                .then(mindTestQuestionBuilder("Излагались все факты в ходе дискуссии"))
                                .then(mindTestQuestionBuilder("Логично и последовательно обрисовывались затрагиваемые спорные вопросы"))
                                .then(mindTestQuestionBuilder("Определялись ценности, которые исповедует та или иная сторона в дискуссии"))
                                .then(mindTestQuestionBuilder("Ярко освещались обе стороны спорного вопроса и существо конфликта"))
                                //16
                                .then(mindTestQuestionBuilder("Заинтересованности в совершенствовании своих профессиональных знаний"))
                                .then(mindTestQuestionBuilder("Указания со стороны уважаемого мной человека на возможную ее полезность"))
                                .then(mindTestQuestionBuilder("Желания расширить свою общую эрудицию"))
                                .then(mindTestQuestionBuilder("Желания выйти за пределы собственной деятельности для разнообразия"))
                                .then(mindTestQuestionBuilder("Стремления узнать больше об определенном предмете"))
                                //17
                                .then(mindTestQuestionBuilder("Пытаться связать ее с более широкой проблемой или теорией"))
                                .then(mindTestQuestionBuilder("Искать пути и способы быстро решить эту проблему"))
                                .then(mindTestQuestionBuilder("Обдумывать альтернативные способы ее решения"))
                                .then(mindTestQuestionBuilder("Искать способы, которыми другие, возможно, уже решили эту проблему"))
                                .then(mindTestQuestionBuilder("Пытаться найти самую лучшую процедуру для ее решения"))
                                //18
                                .then(mindTestQuestionBuilder("Находить существующие методы, которые работают, и использовать их как можно лучше"))
                                .then(mindTestQuestionBuilder("Ломать голову над тем, как разнородные методы могли бы работать вместе"))
                                .then(mindTestQuestionBuilder("Открывать новые и более совершенные методы"))
                                .then(mindTestQuestionBuilder("Находить способы заставить существующие методы работать лучше и по-новому"))
                                .then(mindTestQuestionBuilder("Разбираться в том, как и почему существующие методы должны работать"))
                                .then();
                    }
                    return Mono.empty();
                });
    }

    private Mono<Boolean> checkExistsTestByName(String testName){
        return template.exists(query(where("test_name").is(testName)), Test.class);
    }

    private Mono<Test> insertTest(String testName, String name){
        String description = null;
        switch (testName){
            case belbinTest -> description = "Состоит из 7 отдельных блоков по 8 вопросов или утверждений, с которыми вы можете согласиться или не согласиться. " +
                    "На каждый блок у Вас есть 10 очков. " +
                    "Присваивать очки можно не больше, чем 3м, утверждениям в блоке. " +
                    "При этом одному предложению можно присвоить минимум 2 очка. " +
                    "Если Вы согласны с каким-либо утверждением на все 100%, Вы можете отдать ему все 10 очков. " +
                    "Строго 10 нужно распоеделить.";
            case temperTest -> description = "Вам предлагается ответить на 57 вопросов. " +
                    "Вопросы направлены на выявление вашего обычного способа поведения. " +
                    "Постарайтесь представить типичные ситуации и дайте первый «естественный» ответ, который придет вам в голову. " +
                    "Отвечайте быстро и точно. Помните, что нет «хороших» или «плохих» ответов. " +
                    "Если вы согласны с утверждением, поставьте рядом с его номером знак + (да), если нет — знак — (нет).";
            case mindTest -> description = "Каждый пункт данного опросника состоит из утверждения, за которым следует пять его возможных окончаний. " +
                    "Ваша задача – указать ту степень, в которой каждое окончание применимо к Вам. " +
                    "На бланке ответов напротив каждого окончания проставьте номера: 5, 4, 3, 2, или 1, указывающие на ту степень, в какой данное окончание применимо к Вам: от 5 (более всего подходит) до 1 (менее всего подходит). " +
                    "Каждый номер (балл) должен быть использован только один раз (!!!) в группе из пяти окончаний. " +
                    "(Всего таких групп в опроснике 18). " +
                    "Даже если 2 окончания (или больше) в одной группе покажутся одинаково применимы к Вам, все-таки постарайтесь их упорядочить. " +
                    "Имейте ввиду, что для каждой группы каждый балл (5, 4, 3, 2 или 1) нельзя использовать более одного раза.";
        }
        return template.insert(Test.builder().testName(testName).name(name).description(description).build());
    }

    private String sumBelbinResult(Integer score){
        switch (score){
            case 0 -> { return "Вы РЕАЛИЗАТОР"; }
            case 1 -> { return "Вы КООРДИНАТОР"; }
            case 2 -> { return "Вы МОТИВАТОР"; }
            case 3 -> { return "Вы ГЕНЕРАТОР ИДЕЙ"; }
            case 4 -> { return "Вы ИССЛЕДОВАТЕЛЬ"; }
            case 5 -> { return "Вы АНАЛИТИК-ЭКСПЕРТ"; }
            case 6 -> { return "Вы ВДОХНОВИТЕЛЬ"; }
            case 7 -> { return "Вы КОНТРОЛЕР"; }
        }
        return null;
    }

    private String sumTemperTest(List<Integer> score){
        String result = "Ваш уровень Экстраверсии: (" + score.get(0) + ") ";
        if (score.get(0) < 5) result += "глубокий интроверт";
        else if (score.get(0) < 9) result += "интроверт";
        else if (score.get(0) <= 15) result += "среднее значение";
        else if (score.get(0) <= 19) result += "экстраверт";
        else if (score.get(0) > 19) result += "яркий экстраверт";

        result += "\nВаш уровень Нейротизма: (" + score.get(1) + ") ";
        if (score.get(1) < 7) result += "низкий уровень нейротизма";
        else if (score.get(1) <= 14) result += "среднее значение";
        else if (score.get(1) <= 19) result += "высокий уровень нейротизма";
        else if (score.get(1) > 19) result += "очень высокий уровень нейротизма";

        result += "\nВаш уровень Лжи: (" + score.get(2) + ") ";
        if (score.get(2) <= 4) result += "норма";
        else if (score.get(2) > 4) result += "неискренность в ответах, " +
                "свидетельствующая также о некоторой демонстративности поведения и ориентированности испытуемого на социальное одобрение";

        return result;
    }

    private String sumMindResult(Integer score, String style){
        if (score <= 36){
            return style + ") этот стиль абсолютно чужд испытуемому," +
                    " он, вероятно, не пользуется им практически нигде и никогда, " +
                    "даже если этот стиль является лучшим подходом к проблеме при данных обстоятельствах\n";
        }
        else if (score <= 42){ return style + ") вероятно стойкое игнорирование данного стиля\n"; }
        else if (score <= 48){
            return style + ") для испытуемого характерно умеренное пренебрежение этим стилем мышления, " +
                    "то есть, при прочих равных условиях, он, по возможности, " +
                    "будет избегать использования данного стиля при решении значимых проблем\n";
        }
        else if (score <= 59){ return style + ") зона неопределенности. Данный стиль следует исключить из рассмотрения\n"; }
        else if (score <= 65){
            return style + ") испытуемый отдает умеренное предпочтение этому стилю. " +
                    "Иначе говоря, при прочих равных условиях, " +
                    "он будет предрасположен использовать этот стиль больше или чаще других\n";
        }
        else if (score <= 71){
            return style + ") испытуемый оказывает сильное предпочтение такому стилю мышления. " +
                    "Вероятно, он пользуется данным стилем систематически, последовательно и в большинстве ситуаций. " +
                    "Возможно даже, что время от времени испытуемый злоупотребляет им, " +
                    "то есть использует тогда, стиль не обеспечивает лучший подход к проблеме. " +
                    "Чаще это может происходить в напряженных ситуациях (дефицит времени, конфликт и т.п.)\n";
        }
        else {
            return style + ") у испытуемого очень сильное предпочтение этого стиля мышления. " +
                    "Другими словами, он чрезмерно фиксирован на нем, использует его практически во всех ситуациях, " +
                    "следовательно, и в таких, где этот стиль является далеко не лучшим (или даже неприемлемым) подходом к проблеме\n";
        }
    }

    private String sumMindResult(List<Integer> score){
        return "Синтетический стиль: (" + score.get(0) + ")\nИдеалистический стиль: (" + score.get(1) + ")\nПрагматический стиль: ("
                + score.get(2) + ")\nАналитический стиль: (" + score.get(3) + ")\nРеалистический стиль: (" + score.get(4) + ")";
    }

    private Mono<TestResultDTO> saveResult(List<TestAnswerDTO> answers, TestResult testResult, Boolean needQuestionModuleNumber){
        return template.getDatabaseClient().inConnection(connection -> {
                    Batch batch = connection.createBatch();
                    answers.forEach(r -> {
                                if (Boolean.TRUE.equals(needQuestionModuleNumber)) {
                                    batch.add(
                                            String.format("INSERT INTO test_answer (test_name, user_id, question_name, question_module_number, question_number, answer) VALUES ('%s', '%s', '%s', '%s', '%s', '%s');",
                                                    r.getTestName(), r.getUser().getId(), r.getQuestionName(), r.getQuestionModuleNumber(), r.getQuestionNumber(), r.getAnswer())
                                    );
                                }
                                else {
                                    batch.add(
                                            String.format("INSERT INTO test_answer (test_name, user_id, question_name, question_number, answer) VALUES ('%s', '%s', '%s', '%s', '%s');",
                                                    r.getTestName(), r.getUser().getId(), r.getQuestionName(), r.getQuestionNumber(), r.getAnswer())
                                    );
                                }
                            }
                    );
                    return Mono.from(batch.execute());
                })
                .then(template.insert(testResult)
                        .flatMap(r -> Mono.just(TestResultDTO.builder()
                                .id(r.getId())
                                .testName(r.getTestName())
                                .user(answers.get(0).getUser())
                                .result(r.getTestResult())
                                .build())));
    }

    @Scheduled(fixedRate = 6000000)
    public void checkExistsTest(){
        checkExistsTestByName(belbinTest)
                .flatMap(b -> {
                    if (Boolean.FALSE.equals(b)){
                        return insertTest(belbinTest, "Тест Белбина").then(checkBelbinTestQuestion());
                    }
                    return checkBelbinTestQuestion();
                })
                .then(checkExistsTestByName(temperTest)
                        .flatMap(b -> {
                            if (Boolean.FALSE.equals(b)){
                                return insertTest(temperTest, "Айзенка личностный опросник ").then(checkTemperTestQuestion());
                            }
                            return checkTemperTestQuestion();
                        }))
                .then(checkExistsTestByName(mindTest)
                        .flatMap(b -> {
                            if (Boolean.FALSE.equals(b)){
                                return insertTest(mindTest, "Опросник «Стиль мышления»").then(checkMindTestQuestion());
                            }
                            return checkMindTestQuestion();
                        })).subscribe();
    }

    //get

    public Flux<TestDTO> getAllTest(){
        return template.getDatabaseClient()
                .sql("SELECT * FROM test")
                .map((row, rowMetadata) -> TestDTO.builder()
                        .id(row.get("id", String.class))
                        .testName(row.get("test_name", String.class))
                        .name(row.get("name", String.class))
                        .build())
                .all();
    }

    public Mono<TestDTO> getTest(String testName){
        return template.getDatabaseClient()
                .sql("SELECT * FROM test WHERE test_name =:testName")
                .bind("testName", testName)
                .map((row, rowMetadata) -> TestDTO.builder()
                        .id(row.get("id", String.class))
                        .testName(row.get("test_name", String.class))
                        .name(row.get("name", String.class))
                        .description(row.get("description", String.class))
                        .build())
                .first();
    }

    public Flux<TestQuestionDTO> getTestQuestions(String testName, Integer moduleNumber){
        return template.getDatabaseClient()
                .sql("SELECT * FROM test_question WHERE test_name =:testName AND question_module_number = :moduleNumber")
                .bind("testName", testName)
                .bind("moduleNumber", moduleNumber)
                .map((row, rowMetadata) -> TestQuestionDTO.builder()
                        .id(row.get("id", String.class))
                        .testName(row.get("test_name", String.class))
                        .questionNumber(row.get("question_number", Integer.class))
                        .questionName(row.get("question_name", String.class))
                        .questionModuleNumber(row.get("question_module_number", Integer.class))
                        .questionModule(row.get("question_module", String.class))
                        .question(row.get("question", String.class))
                        .build())
                .all()
                .sort(Comparator.comparing(TestQuestionDTO::getQuestionNumber));
    }

    public Flux<TestResultDTO> getAllResult(String testName){
        String query = """
                SELECT
                    tr.id AS tr_id, tr.user_id AS tr_user_id, tr.test_name AS tr_test_name, tr.test_result AS tr_test_result, tr.score AS tr_score,
                    u.id AS u_id, u.email AS u_email, u.first_name AS u_first_name, u.last_name AS u_last_name
                FROM test_result tr
                LEFT JOIN users u ON u.id = tr.user_id
                WHERE tr.test_name = :testName
                """;
        return template.getDatabaseClient()
                .sql(query)
                .bind("testName", testName)
                .map((row, rowMetadata) -> {
                    TestResultDTO testResultDTO = TestResultDTO.builder()
                            .id(row.get("tr_id", String.class))
                            .testName(row.get("tr_test_name", String.class))
                            .user(UserDTO.builder()
                                    .id(row.get("u_id", String.class))
                                    .email(row.get("u_email", String.class))
                                    .firstName(row.get("u_first_name", String.class))
                                    .lastName(row.get("u_last_name", String.class))
                                    .build())
                            .build();
                    if (Objects.equals(testResultDTO.getTestName(), mindTest)){
                        testResultDTO.setTestResult(sumMindResult(List.of(row.get("tr_score", Integer[].class))));
                    } else { testResultDTO.setResult(row.get("tr_test_result", String.class)); }
                    return testResultDTO;
                })
                .all();
    }

    public Flux<TestAllResponse> getTestGeneral(){
        String query = """
                SELECT
                    tr.id AS tr_id, tr.user_id AS tr_user_id, tr.test_name AS tr_test_name, tr.test_result AS tr_test_result, tr.score AS tr_score,
                    u.id AS u_id, u.email AS u_email, u.first_name AS u_first_name, u.last_name AS u_last_name, u.study_group AS u_study_group
                FROM test_result tr
                LEFT JOIN users u ON u.id = tr.user_id
                """;

        ConcurrentHashMap<String, TestAllResponse> map = new ConcurrentHashMap<>();

        return template.getDatabaseClient()
                .sql(query)
                .map((row, rowMetadata) -> {
                    String userId = row.get("u_id", String.class);
                    String testName = row.get("tr_test_name", String.class);
                    TestAllResponse testAllResponse = map.getOrDefault(userId, TestAllResponse.builder()
                            .user(UserDTO.builder()
                                    .id(row.get("u_id", String.class))
                                    .email(row.get("u_email", String.class))
                                    .firstName(row.get("u_first_name", String.class))
                                    .lastName(row.get("u_last_name", String.class))
                                    .studyGroup(row.get("u_study_group", String.class))
                                    .build())
                            .belbinTest(Objects.equals(testName, belbinTest) ?
                                    TestResultDTO.builder()
                                            .id(row.get("tr_id", String.class))
                                            .testName(testName)
                                            .result(row.get("tr_test_result", String.class))
                                            .build() : null)
                            .mindTest(Objects.equals(testName, mindTest) ?
                                    TestResultDTO.builder()
                                            .id(row.get("tr_id", String.class))
                                            .testName(testName)
                                            .result(sumMindResult(List.of(row.get("tr_score", Integer[].class))))
                                            .build() : null)
                            .temperTest(Objects.equals(testName, temperTest) ?
                                    TestResultDTO.builder()
                                            .id(row.get("tr_id", String.class))
                                            .testName(testName)
                                            .result(row.get("tr_test_result", String.class))
                                            .build() : null)
                            .build());
                    if (Objects.equals(testName, belbinTest) && testAllResponse.getBelbinTest() == null){
                        testAllResponse.setBelbinTest(TestResultDTO.builder()
                                .id(row.get("tr_id", String.class))
                                .testName(testName)
                                .result(row.get("tr_test_result", String.class))
                                .build());
                    }
                    else if (Objects.equals(testName, mindTest) && testAllResponse.getMindTest() == null){
                        testAllResponse.setMindTest(TestResultDTO.builder()
                                .id(row.get("tr_id", String.class))
                                .testName(testName)
                                .result(sumMindResult(List.of(row.get("tr_score", Integer[].class))))
                                .build());
                    }
                    else if (Objects.equals(testName, temperTest) && testAllResponse.getTemperTest() == null){
                        testAllResponse.setTemperTest(TestResultDTO.builder()
                                .id(row.get("tr_id", String.class))
                                .testName(testName)
                                .result(row.get("tr_test_result", String.class))
                                .build());
                    }
                    map.put(userId,testAllResponse);
                    return testAllResponse;
                })
                .all().thenMany(Flux.fromIterable(map.values()));
    }

    public Flux<TestAnswerDTO> getAnswers(String testName, String userId){
        String query = """
                SELECT
                    ta.id AS ta_id, ta.test_name AS ta_test_name, ta.user_id AS ta_user_id, ta.question_name AS ta_question_name,
                    ta.question_module_number AS ta_question_module_number, ta.question_number AS ta_question_number, ta.answer AS ta_answer,
                    u.id AS u_id, u.email AS u_email, u.first_name AS u_first_name, u.last_name AS u_last_name
                FROM test_answer ta
                LEFT JOIN users u ON u.id = ta.user_id
                WHERE ta.test_name = :testName AND ta.user_id = :userId
                """;
        return template.getDatabaseClient()
                .sql(query)
                .bind("testName", testName)
                .bind("userId", userId)
                .map((row, rowMetadata) -> TestAnswerDTO.builder()
                        .id(row.get("ta_id", String.class))
                        .testName(row.get("ta_test_name", String.class))
                        .user(UserDTO.builder()
                                .id(row.get("u_id", String.class))
                                .email(row.get("u_email", String.class))
                                .firstName(row.get("u_first_name", String.class))
                                .lastName(row.get("u_last_name", String.class))
                                .build())
                        .questionName(row.get("ta_question_name", String.class))
                        .questionModuleNumber(row.get("ta_question_module_number", Integer.class))
                        .questionNumber(row.get("ta_question_number", Integer.class))
                        .answer(row.get("ta_answer", String.class))
                        .build())
                .all()
                .sort(Comparator.comparing(TestAnswerDTO::getQuestionNumber));
    }

    public Mono<TestResultDTO> getResult(String testName, String userId){
        String query = """
                SELECT
                    tr.id AS tr_id, tr.user_id AS tr_user_id, tr.test_name AS tr_test_name, tr.test_result AS tr_test_result,
                    u.id AS u_id, u.email AS u_email, u.first_name AS u_first_name, u.last_name AS u_last_name
                FROM test_result tr
                LEFT JOIN users u ON u.id = tr.user_id
                WHERE tr.test_name = :testName AND tr.user_id = :userId
                """;
        return template.getDatabaseClient()
                .sql(query)
                .bind("testName", testName)
                .bind("userId", userId)
                .map((row, rowMetadata) -> TestResultDTO.builder()
                        .id(row.get("tr_id", String.class))
                        .testName(row.get("tr_test_name", String.class))
                        .user(UserDTO.builder()
                                .id(row.get("u_id", String.class))
                                .email(row.get("u_email", String.class))
                                .firstName(row.get("u_first_name", String.class))
                                .lastName(row.get("u_last_name", String.class))
                                .build())
                        .result(row.get("tr_test_result", String.class))
                        .build())
                .first();
    }

    public Mono<InputStreamResource> generateFile(String testName) {
        String query = """
                SELECT
                    tr.id AS tr_id, tr.user_id AS tr_user_id, tr.test_name AS tr_test_name, tr.test_result AS tr_test_result, tr.score AS tr_score,
                    u.id AS u_id, u.email AS u_email, u.first_name AS u_first_name, u.last_name AS u_last_name, u.study_group AS u_study_group
                FROM test_result tr
                LEFT JOIN users u ON u.id = tr.user_id
                WHERE tr.test_name = :testName
                """;
        return template.getDatabaseClient()
                .sql(query)
                .bind("testName", testName)
                .map((row, rowMetadata) -> {
                    TestResultDTO testResultDTO = TestResultDTO.builder()
                            .id(row.get("tr_id", String.class))
                            .testName(row.get("tr_test_name", String.class))
                            .user(UserDTO.builder()
                                    .id(row.get("u_id", String.class))
                                    .email(row.get("u_email", String.class))
                                    .firstName(row.get("u_first_name", String.class))
                                    .lastName(row.get("u_last_name", String.class))
                                    .studyGroup(row.get("u_study_group", String.class))
                                    .build())
                            .build();
                    if (Objects.equals(testResultDTO.getTestName(), mindTest)){
                        testResultDTO.setTestResult(sumMindResult(List.of(row.get("tr_score", Integer[].class))).replace("\n", " "));
                    } else {
                        testResultDTO.setResult(row.get("tr_test_result", String.class).replace("\n", " "));
                    }
                    return testResultDTO;
                })
                .all()
                .collectList()
                .flatMap(l -> {
                    ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                    try (PrintWriter writer = new PrintWriter(byteArrayOutputStream)) {
                        l.forEach(r -> writer.println(r.getUser().getFirstName() + " "
                                + r.getUser().getLastName() + "\t"
                                + r.getUser().getStudyGroup() + "\t"
                                + r.getResult()));
                    }
                    return Mono.just(new InputStreamResource(new ByteArrayInputStream(byteArrayOutputStream.toByteArray())));
                });
    }

    //post
    public Mono<TestResultDTO> testBelbinResult(String userId, Flux<TestAnswerDTO> answers){
        TestResult testResult = new TestResult();
        testResult.setTestName(belbinTest);
        testResult.setUserId(userId);
        testResult.setScore(new ArrayList<>(List.of(0, 0, 0, 0, 0, 0, 0, 0)));
        return answers.collectList()
                .flatMap(l -> {
                    l.forEach(r -> {
                        switch (r.getQuestionNumber()) {
                            case 16, 20, 37, 43, 51, 65, 74 -> testResult.getScore().set(0, testResult.getScore().get(0) + Integer.parseInt(r.getAnswer()));
                            case 13, 21, 30, 47, 55, 62, 76 -> testResult.getScore().set(1, testResult.getScore().get(1) + Integer.parseInt(r.getAnswer()));
                            case 15, 24, 32, 41, 53, 66, 70 -> testResult.getScore().set(2, testResult.getScore().get(2) + Integer.parseInt(r.getAnswer()));
                            case 12, 26, 33, 44, 57, 60, 75 -> testResult.getScore().set(3, testResult.getScore().get(3) + Integer.parseInt(r.getAnswer()));
                            case 10, 22, 35, 46, 54, 67, 73 -> testResult.getScore().set(4, testResult.getScore().get(4) + Integer.parseInt(r.getAnswer()));
                            case 17, 23, 36, 42, 50, 64, 71 -> testResult.getScore().set(5, testResult.getScore().get(5) + Integer.parseInt(r.getAnswer()));
                            case 11, 25, 34, 40, 52, 61, 77 -> testResult.getScore().set(6, testResult.getScore().get(6) + Integer.parseInt(r.getAnswer()));
                            case 14, 27, 31, 45, 56, 63, 72 -> testResult.getScore().set(7, testResult.getScore().get(7) + Integer.parseInt(r.getAnswer()));
                        }
                    });
                    if (testResult.getScore().stream().mapToInt(Integer::intValue).sum() != 70) {
                        return Mono.error(new ServerProcessException("Сумма ответов не равна 70"));
                    }
                    testResult.setTestResult(sumBelbinResult(testResult.getScore().indexOf(Collections.max(testResult.getScore()))));
                    return saveResult(l, testResult, Boolean.TRUE);
                });
    }

    public Mono<TestResultDTO> testTemperResult(String userId, Flux<TestAnswerDTO> answers){
        TestResult testResult = new TestResult();
        testResult.setTestName(temperTest);
        testResult.setUserId(userId);
        testResult.setScore(new ArrayList<>(List.of(0, 0, 0)));
        return answers.collectList()
                .flatMap(l -> {
                    l.forEach(r -> {
                        if (Objects.equals(r.getAnswer(), "+")) {
                            switch (r.getQuestionNumber()){
                                case 1, 3, 8, 10, 13, 17, 22, 25, 27, 39, 44, 46, 49, 53, 56 ->
                                        testResult.getScore().set(0, testResult.getScore().get(0) + 1);
                                case 2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40, 43, 45, 47, 50, 52, 55, 57 ->
                                        testResult.getScore().set(1, testResult.getScore().get(1) + 1);
                                case 6, 24, 36 ->
                                        testResult.getScore().set(2, testResult.getScore().get(2) + 1);
                            }
                        }
                        else if (Objects.equals(r.getAnswer(), "-")) {
                            switch (r.getQuestionNumber()){
                                case 5, 15, 20, 29, 32, 34, 37, 41, 51 ->
                                        testResult.getScore().set(0, testResult.getScore().get(0) + 1);
                                case 12, 18, 30, 42, 48, 54 ->
                                        testResult.getScore().set(2, testResult.getScore().get(2) + 1);
                            }
                        }
                    });
                    testResult.setTestResult(sumTemperTest(testResult.getScore()));
                    return saveResult(l, testResult, Boolean.FALSE);
                });
    }

    public Mono<TestResultDTO> testMindResult(String userId, Flux<TestAnswerDTO> answers){
        TestResult testResult = new TestResult();
        testResult.setTestName(mindTest);
        testResult.setUserId(userId);
        testResult.setScore(new ArrayList<>(List.of(0, 0, 0, 0, 0)));
        return answers.collectList()
                .flatMap(l -> {
                    l.forEach(r -> {
                        switch (r.getQuestionModuleNumber()){
                            case 1, 7, 13 -> {
                                switch (r.getQuestionNumber()){
                                    case 1 -> testResult.getScore().set(0, testResult.getScore().get(0) + Integer.parseInt(r.getAnswer()));
                                    case 2 -> testResult.getScore().set(1, testResult.getScore().get(1) + Integer.parseInt(r.getAnswer()));
                                    case 3 -> testResult.getScore().set(2, testResult.getScore().get(2) + Integer.parseInt(r.getAnswer()));
                                    case 4 -> testResult.getScore().set(3, testResult.getScore().get(3) + Integer.parseInt(r.getAnswer()));
                                    case 5 -> testResult.getScore().set(4, testResult.getScore().get(4) + Integer.parseInt(r.getAnswer()));
                                }
                            }
                            case 2, 8, 14 -> {
                                switch (r.getQuestionNumber()){
                                    case 1 -> testResult.getScore().set(1, testResult.getScore().get(1) + Integer.parseInt(r.getAnswer()));
                                    case 2 -> testResult.getScore().set(0, testResult.getScore().get(0) + Integer.parseInt(r.getAnswer()));
                                    case 3 -> testResult.getScore().set(3, testResult.getScore().get(3) + Integer.parseInt(r.getAnswer()));
                                    case 4 -> testResult.getScore().set(2, testResult.getScore().get(2) + Integer.parseInt(r.getAnswer()));
                                    case 5 -> testResult.getScore().set(4, testResult.getScore().get(4) + Integer.parseInt(r.getAnswer()));
                                }
                            }
                            case 3, 9, 15 -> {
                                switch (r.getQuestionNumber()){
                                    case 1 -> testResult.getScore().set(2, testResult.getScore().get(2) + Integer.parseInt(r.getAnswer()));
                                    case 2 -> testResult.getScore().set(4, testResult.getScore().get(4) + Integer.parseInt(r.getAnswer()));
                                    case 3 -> testResult.getScore().set(3, testResult.getScore().get(3) + Integer.parseInt(r.getAnswer()));
                                    case 4 -> testResult.getScore().set(1, testResult.getScore().get(1) + Integer.parseInt(r.getAnswer()));
                                    case 5 -> testResult.getScore().set(0, testResult.getScore().get(0) + Integer.parseInt(r.getAnswer()));
                                }
                            }
                            case 4, 10, 16 -> {
                                switch (r.getQuestionNumber()){
                                    case 1 -> testResult.getScore().set(3, testResult.getScore().get(3) + Integer.parseInt(r.getAnswer()));
                                    case 2 -> testResult.getScore().set(4, testResult.getScore().get(4) + Integer.parseInt(r.getAnswer()));
                                    case 3 -> testResult.getScore().set(1, testResult.getScore().get(1) + Integer.parseInt(r.getAnswer()));
                                    case 4 -> testResult.getScore().set(0, testResult.getScore().get(0) + Integer.parseInt(r.getAnswer()));
                                    case 5 -> testResult.getScore().set(2, testResult.getScore().get(2) + Integer.parseInt(r.getAnswer()));
                                }
                            }
                            case 5, 11, 17 -> {
                                switch (r.getQuestionNumber()){
                                    case 1 -> testResult.getScore().set(1, testResult.getScore().get(1) + Integer.parseInt(r.getAnswer()));
                                    case 2 -> testResult.getScore().set(2, testResult.getScore().get(2) + Integer.parseInt(r.getAnswer()));
                                    case 3 -> testResult.getScore().set(0, testResult.getScore().get(0) + Integer.parseInt(r.getAnswer()));
                                    case 4 -> testResult.getScore().set(4, testResult.getScore().get(4) + Integer.parseInt(r.getAnswer()));
                                    case 5 -> testResult.getScore().set(3, testResult.getScore().get(3) + Integer.parseInt(r.getAnswer()));
                                }
                            }
                            case 6, 12, 18 -> {
                                switch (r.getQuestionNumber()){
                                    case 1 -> testResult.getScore().set(4, testResult.getScore().get(4) + Integer.parseInt(r.getAnswer()));
                                    case 2 -> testResult.getScore().set(0, testResult.getScore().get(0) + Integer.parseInt(r.getAnswer()));
                                    case 3 -> testResult.getScore().set(1, testResult.getScore().get(1) + Integer.parseInt(r.getAnswer()));
                                    case 4 -> testResult.getScore().set(2, testResult.getScore().get(2) + Integer.parseInt(r.getAnswer()));
                                    case 5 -> testResult.getScore().set(3, testResult.getScore().get(3) + Integer.parseInt(r.getAnswer()));
                                }
                            }
                        }
                    });
                    testResult.setTestResult(sumMindResult(testResult.getScore().get(0), "Синтетический стиль: (" + testResult.getScore().get(0)));
                    testResult.setTestResult(sumMindResult(testResult.getScore().get(1), testResult.getTestResult() + "Идеалистический стиль: (" + testResult.getScore().get(1)));
                    testResult.setTestResult(sumMindResult(testResult.getScore().get(2), testResult.getTestResult() + "Прагматический стиль: (" + testResult.getScore().get(2)));
                    testResult.setTestResult(sumMindResult(testResult.getScore().get(3), testResult.getTestResult() + "Аналитический стиль: (" + testResult.getScore().get(3)));
                    testResult.setTestResult(sumMindResult(testResult.getScore().get(4), testResult.getTestResult() + "Реалистический стиль: (" + testResult.getScore().get(4)));
                    return saveResult(l, testResult, Boolean.TRUE);
                });
    }
}
