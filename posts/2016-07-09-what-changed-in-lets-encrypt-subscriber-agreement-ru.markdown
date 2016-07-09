----
title: Что поменялось в Let's Encrypt Subscriber Agreement
language: russian
description: Человекочитаемый перечень изменений с цитатами старых и новых версий пунктов соглашения
tags: EULA
----

<i>**Дисклеймер**: я не юрист, а этот документ не является юридической
консультацией или чем-либо ещё. Мои парафразы могут быть неточными или
неверными.</i>

Let's Encrypt только что выкатил [новую версию][new-sa] Subscriber Agreement,
которая вступит в действие первого августа. Несмотря на молодость
и прогрессивность инициативы, текст документа готовится в Word, а при
публикации просто конвертируется в PDF — ни перечня изменений тебе, ничего
нет. Сообщество [уже ноет][crying] по этому поводу на форуме, а более
инициативные личности выкладывают [текстовые версии соглашения][txt] и [гитовые
диффы документов][git-diff].

К сожалению, вывод `git diff` (даже с `--word-diff`)
временами все равно сложно разобрать, поэтому я решил сэкономить вам усилия
и подготовить более человекочитаемый перечень изменений.

[new-sa]: https://letsencrypt.org/documents/LE-SA-v1.1.1-August-1-2016.pdf
    "Let's Encrypt Subscriber Agreement Version 1.1.1"

[crying]: https://community.letsencrypt.org/t/subscriber-agreement-1-0-1-1-1-1-change-please-publish-a-diff/17740
    "Subscriber agreement 1.0.1 -> 1.1.1 change - please publish a diff"

[txt]: https://www.osso.nl/blog/letsencrypt-license-update-show-differences/
    "letsencrypt / license update / show differences"

[git-diff]: https://gist.github.com/kennwhite/9541c8f37ec2994352c4554ca2afeece/revisions

Итак, что же изменилось в соглашении?

* Добавлено понятие «компрометирования ключа»:

    > “Key Compromise”— A Private Key is said to be compromised if its value has
    > been disclosed to an unauthorized person, an unauthorized person has had
    > access to it, or there exists a practical technique by which an
    > unauthorized person may discover its value. A Private Key is also
    > considered compromised if methods have been developed that can easily
    > calculate it based on the Public Key or if there is clear evidence that the
    > specific method used to generate the Private Key was flawed.

* в пункте 3.1 («Warranties») в пунктах списка начиная с третьего добавлено
  уточнение касательно того, кому именно эти самые гарантии предоставляются —
  «to ISRG and the public-at-large»;

* в четвёртом пункте списка в пункте 3.1 добавлено уточнение касательно
  предоставляемой в ISRG информации — вместо «is» там теперь «is, and You agree
  that all information you will provide to ISRG at any time will be,».

    Было:

    > You warrant that all information You have provided to ISRG is accurate,
    > current, complete, reliable, complete, and not misleading.

    Стало:

    > You warrant to ISRG and the public-at-large that all information You have
    > provided to ISRG is, and You agree that all information you will provide to
    > ISRG at any time will be, accurate, current, complete, reliable, and not
    > misleading.

* последние два пункта списка в пункте 3.1 объединены; добавлено такое же
  уточнение, как и в четвёртый; а также ужесточены требования к сохранности
  связанных с ключом данных.

    Было:

    > * You warrant that You have taken all appropriate, reasonable, and
    >   necessary steps to secure and keep your Private Key secret.
    >
    > * You warrant that You will not use Your Certificates to attack, defraud or
    >   intercept the traffic of others.

    Стало:

    > * You warrant to ISRG and the public-at-large that You have taken, and You
    >   agree that at all times You will take, all appropriate, reasonable, and
    >   necessary steps to maintain sole control of, secure, properly protect and
    >   keep secret and confidential the Private Key corresponding to the Public
    >   Key in Your Certificate (and any associated activation data or device,
    >   e.g. password or token).

* в пункте 3.4 («Key Pair Generation») теперь явно указано, что созданные вашей
  машиной приватный и публичный ключи остаются вашей собственностью;

* в пункте 3.5 («Inspection and Acceptance of Certificates») вы теперь не
  только *соглашаетесь*, но и *гарантируете* («to ISRG and the
  public-at-large», конечно же), что осуществите проверку только что созданных
  сертификатов и сразу же отзовёте их, если вдруг найдёте неточности;

* пункт 3.6 переименован из «Use of Your Certificate» в «Installation and Use
  of Your Certificate»; добавлен новый абзац:

    > You may reproduce and distribute Your Certificate on a nonexclusive and
    > royalty-free basis, provided that it is reproduced and distributed in full
    > and in compliance with this Agreement. You warrant to ISRG and the
    > public-at-large, and You agree, that You will install Your Certificate only
    > on servers that are accessible at the subjectAltName(s) listed in Your
    > Certificate, and that you will use Your Certificate solely in compliance
    > with all applicable laws and solely in accordance with this Agreement. Your
    > Certificate will remain the property of ISRG, subject to Your right to use
    > it as set forth in this Agreement.

    В уже существовавшем абзаце добавлено уточнение, что сертификаты служат не
    только для шифрования, но и для аутентификации;

* в пункте 3.7 («When to Revoke Your Certificate») немного перефразированы
  пункты списка. Просто сравните, это быстрей, чем читать любой мой пересказ.

    Было (форматирование моё):

    > You must immediately request that Your Certificate be revoked if:
    >
    > (i) You suspect or discover that Your Private Key has been, or is in danger
    > of being, lost, stolen, otherwise compromised, or subjected to unauthorized
    > use, or
    >
    > (ii) any information in Your Certificate is no longer accurate, current or
    > complete, or any such information becomes misleading.

    Стало (форматирование моё):

    > You warrant to ISRG and the public-at-large, and You agree, that You
    > will immediately request that Your Certificate be revoked if:
    >
    > (i) there is any actual or suspected misuse or Key Compromise of the
    > Private Key associated with the Public Key included in Your Certificate, or
    >
    > (ii) any information in Your Certificate is, or becomes, misleading,
    > incorrect or inaccurate.

    Там есть ещё пара предложений, но они не изменились;

* в пункте 3.8 («When to Cease Using Your Certificate») произошли упрощения:
  вместо того, чтобы повторяться про компромисс ключа и прочее, теперь просто
  указано очевидное «не пользуйтесь сертификатом, который отозвали».

    Было (форматирование моё):

    > You must immediately cease using Your Certificate if: 
    >
    > (i) You suspect or discover that the Private Key corresponding to Your
    > Certificate has been or may be stolen, lost, or otherwise compromised or
    > subjected to unauthorized use,
    >
    > (ii) any information in Your Certificate is no longer accurate, current or
    > complete, or any such information becomes misleading, or
    >
    > (iii) upon the revocation or expiration of Your Certificate.

    Стало (форматирование моё):

    > You warrant to ISRG and the public-at-large, and You agree, that You will
    > promptly cease using Your Certificate 
    >
    > (i) if any information in Your Certificate is, or becomes, misleading,
    > incorrect or inaccurate, or
    >
    > (ii) upon the revocation or expiration of Your Certificate.

* старый пункт 3.9 («Indemnification») теперь имеет номер 3.10;

* под номером 3.9 теперь новый пункт — «When to Cease Using Your Private Key»:

    > You warrant to ISRG and the public-at-large, and You agree, that You
    > will promptly cease all use of the Private Key corresponding to the
    > Public Key included in Your Certificate upon revocation of Your
    > Certificate for reasons of known or suspected Key Compromise.

* пункт 4.1 («Privacy») был существенно укорочен и теперь является отсылкой
  к [Let’s Encrypt Privacy Policy][le-repository]:

    > Because others may rely on your use of Your Certificates to encrypt
    > Internet communications, much of the information You send to ISRG will
    > be published by ISRG and will become a matter of public record. ISRG’s
    > collection, storage, use and disclosure of such information are
    > governed by the Let’s Encrypt Privacy Policy at:
    > https://letsencrypt.org/privacy/.

* пункт 4.3 («Suspension and Revocation») теперь начинается со слов «You
  acknowledge and accept that», подчёркивая тот факт, что это соглашение, а не
  просто информация к сведению.

    Также убрана фраза о восстановлении сертификата — даже если позже выяснится,
    что сертификат отозвали зря, его уже не вернут.

    Во втором абзаце пункта зачем-то подчёркивают, что сертификат может быть
    отозван немедленно. Было:

    > ISRG may also, without advance notice, revoke Your Certificate if …

    Стало:

    > You also acknowledge and accept that ISRG may, without advance notice,
    > immediately revoke Your Certificate if …

    Там же в пункте (v) сделано уточнение о том, что условие касается в том числе
    и текущего документа. Было:

    > (v) You have violated any applicable law, agreement or other obligation

    Стало:

    > (v) You have violated any applicable law, agreement (including this
    > Agreement), or other obligation

    Посреди списка условий добавлены два новых пункта, остальные, соответственно,
    сдвинулись. Выглядят новые пункты так (форматирование моё):

    > (vi) Your Certificate is being used, or has been used, to enable any
    > criminal activity (such as phishing attacks, fraud or the distribution of
    > malware);
    >
    > (vii) Your Certificate is being used, or has been used, to intercept the
    > traffic of others;

* в пункте 4.4 («IMPORTANT DISCLAIMER OF WARRANTIES AND LIMITATION OF
  LIABILITY») первые два предложения объединены в одно, а в начало добавлены
  отсылки к [ISRG Certificate Policy][le-repository] и [ISRG Certificate
  Practice Statement][le-repository].

    Было:

    > LET’S ENCRYPT CERTIFICATES AND SERVICES ARE PROVIDED “AS-IS.”
    > ISRG DISCLAIMS ANY AND ALL WARRANTIES …

    Стало:

    > EXCEPT AS EXPRESSLY SET FORTH IN ISRG’S CERTIFICATE POLICY AND
    > CERTIFICATE PRACTICE STATEMENT, LET’S ENCRYPT CERTIFICATES AND
    > SERVICES ARE PROVIDED “AS-IS” AND ISRG DISCLAIMS ANY AND ALL
    > WARRANTIES …

Вот и всё: полный перечень различий между версиями 1.0.1 и 1.1.1
пользовательского соглашения Let's Encrypt.

[le-repository]: https://letsencrypt.org/repository/
    "Let's Encrypt: Policy and Legal Repository"
