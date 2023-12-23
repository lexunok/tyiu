<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Уведомление от портала HITS</title>
    <style>
        body, h2, p {
            padding: 0;
            margin: 0;
        }
        .content {
            height: inherit;
            width: inherit;
            background-color: #f5f5f5;
        }

        .notification-wrapper {
            padding: 16px;
            margin: auto;

            max-width: 400px;

            display: block;
        }

        .notification {
            background-color: #ffffff;
            border-radius: 4px;

            overflow: hidden;
        }

        .notification__header {
            padding: 16px;

            background-color: #0d6efd;

            color: #ffffff;
        }

        .notification__title {
            margin-bottom: 16px;

            font-size: 18px;
            font-weight: 600;
        }

        .notification__content {
            padding: 16px;
        }

        .notification__greetings {
            margin-bottom: 4px;
        }

        .notification__link-wrapper {
            margin-top: 16px;
        }

        .notification__link {
            margin: auto;
            padding: 8px;

            border-radius: 4px;
            background-color: #0d6efd;

            text-decoration: none;
            
            cursor: pointer;

            display: table;
        }

        .notification__footer {
            padding-bottom: 16px;
            padding-top: 4px;
            margin-left: 16px;
            margin-right: 16px;

            border-top: 1px solid #dbdada;

            text-align: center;
        }
    </style>
</head>
<body>
    <div class="content">

        <div class="notification-wrapper">
            <div class="notification">
                <h2 class="notification__header">HITS</h2>

                <div class="notification__content">
                    <#if (notification.title)??>
                      <p class="notification__title">${notification.title}</p>
                    </#if>

                    <p class="notification__greetings">Привет!</p>

                    <#if (notification.message)??>
                      <p>${notification.message}</p>
                    </#if>

                    <#if (notification.link)?? && (notification.buttonName)??>
                      <div class="notification__link-wrapper">
                          <a
                            class="notification__link"
                            href=${notification.link}
                            style="color: #ffffff;"
                          >
                            ${notification.buttonName}
                          </a>
                      </div>
                    </#if>
                </div>

                <div class="notification__footer">
                    <p>Спасибо за уделенное время,</p>
                    <p>С уважением команда HITS</p>
                </div>
            </div>
        </div>

    </div>
</body>
</html>