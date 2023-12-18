<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>HTML Template</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <style>
      body {
        font-family: Arial, Helvetica, sans-serif;
        width: 100% !important;
        -webkit-text-size-adjust: 100%;
        -ms-text-size-adjust: 100%;
        margin: 0;
        padding: 0;
        line-height: 100%;
      }

      [style*="Open Sans"] {
        font-family: "Open Sans", arial, sans-serif !important;
      }

      img {
        outline: none;
        text-decoration: none;
        border: none;
        -ms-interpolation-mode: bicubic;
        max-width: 100% !important;
        margin: 0;
        padding: 0;
        display: block;
      }

      table td {
        border-collapse: collapse;
      }

      table {
        border-collapse: collapse;
        mso-table-lspace: 0pt;
        mso-table-rspace: 0pt;
      }

      .all {
        padding: 20px;
      }

      .header-td {
        background: linear-gradient(to bottom left, rgb(161, 91, 226), #0d6efd);
        border-radius: 8px 8px 0 0;
        padding: 20px;
        color: #ffffff;
        font-size: 52px;
        font-weight: 500;
      }

      .logo {
        padding-top: 8px;
        padding-bottom: 8px;
      }

      .title-td {
        padding: 30px;
        font-size: 24px;
        font-weight: 300;
      }

      .title {
        padding-top: 8px;
        padding-bottom: 8px;
      }

      .hello-td {
        padding: 10px;
        font-size: 16px;
      }

      .system-text-td {
        padding: 10px;
        font-size: 16px;
      }

      .sender-text-td {
        padding: 10px;
        font-size: 16px;
        font-style: italic;
      }

      .button-td {
        padding: 32px;
        font-size: 20px;
        font-weight: 400;
      }

      .button-click {
        padding-left: 16px;
        padding-right: 16px;
        padding-top: 12px;
        padding-bottom: 12px;
        border-radius: 4px;
        background-color: #0d6efd;
        text-decoration: none;
        color: white;
      }

      .thanks-td {
        padding: 10px;
        font-size: 16px;
        border-radius: 0 0 8px 8px;
      }

      .thanks {
        padding-top: 10px;
        padding-bottom: 10px;
      }
</style>
</head>

<body style="margin: 0; padding: 0">
      <div
      style="
        font-size: 0px;
        font-color: #ffffff;
        opacity: 0;
        visibility: hidden;
        width: 0;
        height: 0;
        display: none;
      "
    >
    </div>

    <table cellpadding="0" cellspacing="0" width="100%" bgcolor="#EBEBEB">
      <tr>
        <td class="all">
          <table
            class="main table-600"
            cellpadding="0"
            cellspacing="0"
            width="600"
            align="center"
          >
            <tr>
              <td height="30" width="600"></td>
            </tr>

            <tr>
              <td bgcolor="#ffffff" class="header-td">
                <table
                  class="table-528"
                  cellpadding="0"
                  cellspacing="0"
                  width="528"
                  align="center"
                >
                  <tr>
                    <td align="left" class="logo">HITS</td>
                  </tr>
                </table>
              </td>
            </tr>

            <#if (notification.title)??>
                  <tr>
                    <td bgcolor="#ffffff" class="title-td">
                      <table
                        class="table-528"
                        cellpadding="0"
                        cellspacing="0"
                        width="528"
                        align="center"
                      >
                        <tr>
                          <td align="left" class="title">
                             ${notification.title}
                          </td>
                        </tr>
                      </table>
                    </td>
                  </tr>
            </#if>

            <tr>
              <td bgcolor="#ffffff" class="hello-td">
                <table
                  class="table-528"
                  cellpadding="0"
                  cellspacing="0"
                  width="528"
                  align="center"
                >
                  <tr>
                    <td align="left" class="hello">Привет!</td>
                  </tr>
                </table>
              </td>
            </tr>


            <#if (notification.message)??>
                <tr>
                  <td bgcolor="#ffffff" class="system-text-td">
                    <table
                      class="table-528"
                      cellpadding="0"
                      cellspacing="0"
                      width="528"
                      align="center"
                    >
                      <tr>
                        <td align="left" class="system-text">
                            ${notification.message}
                        </td>
                      </tr>
                    </table>
                  </td>
                </tr>
            </#if>

            <#if (notification.link)??>
                <tr>
                  <td bgcolor="#ffffff" class="button-td">
                    <table
                      class="table-528"
                      cellpadding="0"
                      cellspacing="0"
                      width="528"
                      align="center"
                    >
                      <tr>
                        <td align="center">
                          <a
                            href=${notification.link}
                            class="button-click"
                            style="color: white"
                            >Перейти по ссылке</a
                          >
                        </td>
                      </tr>
                    </table>
                  </td>
                </tr>
            </#if>

            <tr>
              <td bgcolor="#ffffff" class="thanks-td">
                <table
                  class="table-528"
                  cellpadding="0"
                  cellspacing="0"
                  width="528"
                  align="center"
                >
                  <tr>
                    <td align="left" class="thanks">
                      Спасибо за уделенное время,
                      <br />
                      С уважением команда HITS
                    </td>
                  </tr>
                </table>
              </td>
            </tr>
          </table>
        </td>
      </tr>
    </table>
  </body>
</html>