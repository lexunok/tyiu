<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title></title>
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

        .change-data-wrapper {
          padding: 16px;
          margin: auto;

          max-width: 400px;

          display: block;
        }

        .change-data {
          background-color: #ffffff;
          border-radius: 4px;

          overflow: hidden;
        }

        .change-data__header {
          padding: 16px;

          background-color: #0d6efd;

          color: #ffffff;
        }

        .change-data__title {
          margin-bottom: 16px;

          font-size: 18px;
          font-weight: 600;
        }

        .change-data__content {
          padding: 16px;
        }

        .change-data__greetings {
          margin-bottom: 4px;
        }

        .change-data__message {
          margin-bottom: 16px;
        }

        .change-data__code {
          margin: auto;

          letter-spacing: 2px;
          font-size: 18px;
          font-weight: 600;

          display: table;
        }

        .change-data__footer {
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

        <div class="change-data-wrapper">
            <div class="change-data">
                <h2 class="change-data__header">HITS</h2>

                <div class="change-data__content">
                    <#if (changeData.subject)??>
                      <p class="change-data__title">${changeData.subject}</p>
                    </#if>

                    <p class="change-data__greetings">Привет!</p>

                    <#if (changeData.text)??>
                      <p class="change-data__message">${changeData.text}</p>
                    </#if>

                    <#if (changeData.code)??>
                      <p class="change-data__code">${changeData.code}</p>
                    </#if>
                </div>

                <div class="change-data__footer">
                    <p>Спасибо за уделенное время,</p>
                    <p>С уважением команда HITS</p>
                </div>
            </div>
        </div>

    </div>
</body>
</html>