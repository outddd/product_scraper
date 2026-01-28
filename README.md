# Product Scraper

Веб-сервер для периодического сбора данных о продуктах с внешнего API.
Сборка и запуск проверены на erlang@27

## Сборка
```bash
make
```

## Запуск
```bash
make run
```
или вручную
```erlang
make shell
application:ensure_all_started(product_scraper).
```

Сервер запустится на http://localhost:8080

## Тесты
```bash
make tests
```

## Логи
```
Хранятся в logs/product_scraper.log
```

## API

### GET /assembled-products

Получить собранные продукты.
```bash
curl http://localhost:8080/assembled-products
curl "http://localhost:8080/assembled-products?start_date=2025-01-01&end_date=2025-12-31"
curl "http://localhost:8080/assembled-products?id=1"
```

### POST /set-time

Изменить интервал сбора (в секундах).
```bash
curl -X POST "http://localhost:8080/set-time?interval=60"
```