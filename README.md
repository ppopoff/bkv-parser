# Bkv parser

## English
The Parser for configuration file format. Implemented in Parboiled2 for my article for Habrahabr and
upcoming scala-talk.

The BKV (Block Key Value) format looks like this:

    server.name = "webserver"
    server {
      port = "8080"
      address = "192.168.88.88"

      settings {
        greeting_message = "Hello!\n It's me!"
      }
    }
    
All Strings must be quoted. Escapings are identical to C-like languages. Lines are separated with Newline.
Keys and Block names may be alphanumeric strings with underscore and dot allowed.

## Русский 
Парсер для формата конфигурационных файлов. Написан с помощью библиотеки Parboiled2, для моей статьи
на habrahabr, а также для предстоящего scala-talk (посвященного Parboiled2)

Формат BKV (Block Key Value) (англ. Блок Ключ Значение), выглядит следующим образом:

    server.name = "webserver"
    server {
      port = "8080"
      address = "192.168.88.88"

      settings {
        greeting_message = "Hello!\n It's me!"
      }
    }
    
Все строки должны быть заключены в двойные кавычки. Экранирование выполняется в стиле присущем многим
С-подобным языкам. Блоки и Пары ключ-значения разделяются переводом строки. Имена блоков и строк могут
содержать символы латиницы, цифры, а так же точку и знак нижнего подчеркивания.
