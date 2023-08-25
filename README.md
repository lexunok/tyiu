ДЛЯ ФРОНТЕНДЕРОВ
---------------------------------------
Вам нужен только файл docker-compose-start.yml который я могу скинуть
и прописать команду
docker-compose -f docker-compose-start.yml up -d
---------------------------------------

В первую очередь запуск контейнеров Docker в cmd
docker compose up -d

Далее запускайте CornApplication
/src/main/java/com/tyiu/corn


ДЛЯ ТЕХ У КОГО НЕТ INTELIJ 
--------------------------------------
      Сборка проекта в главной директории
      ./gradlew clean build
      
      Запуск jar файла
      java -jar /build/libs/corn-1.jar
--------------------------------------


