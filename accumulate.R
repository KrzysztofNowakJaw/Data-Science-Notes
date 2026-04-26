library(tidyverse)
library(readxl)

# --- Wczytanie danych ---
invoices <- tibble(
  Customer = c("A", "A", "A", "B", "B"),
  Invoice  = c("INV1", "INV2", "INV3", "INV4", "INV5"),
  Amount   = c(100, 150, 200, 100, 150)
)

payments <- tibble(
  Customer = c("A", "B"),
  Payment  = c(220, 150)
)

# --- Funkcja alokująca płatność do jednej faktury ---
# acc: c(pozostala_platnosc, ...) — akumulator to wektor (pozostalo + juz przetworzone sumy)
# invoice_amount: kwota biezacej faktury (pojedyncza liczba)
allocate <- function(acc, invoice_amount) {
  remaining_payment <- acc[1]
  allocated         <- min(remaining_payment, invoice_amount)
  new_remaining     <- remaining_payment - allocated
  c(new_remaining, allocated)
}

# --- Główna logika z accumulate ---
result <- invoices |>
  left_join(payments, by = "Customer") |>
  group_by(Customer) |>
  mutate(
    # accumulate() iteruje po Amount każdej faktury
    # akumulator zaczyna od c(Payment, 0) — (pozostała płatność, placeholder)
    # zwraca listę wektorów c(remaining, allocated) dla każdego kroku
    alloc_steps = accumulate(
      Amount,
      allocate,
      .init = c(Payment[1], 0)   # .init = startowy akumulator (pojedyncza wartość: cała płatność)
    )[-1],                        # [-1] usuwa krok startowy (.init)

    Allocated = map_dbl(alloc_steps, \(x) x[2]),
    Remaining = Amount - Allocated
  ) |>
  ungroup() |>
  select(Customer, Invoice, Amount, Allocated, Remaining)

print(result)



-- Challenge 114: Allocate Payments to Invoices
-- Dla każdego klienta płatność jest rozdzielana kolejno na faktury (od pierwszej)

WITH invoices AS (
    SELECT *,
           ROW_NUMBER() OVER (PARTITION BY customer ORDER BY invoice) AS rn
    FROM (VALUES
        ('A', 'INV1', 100),
        ('A', 'INV2', 150),
        ('A', 'INV3', 200),
        ('B', 'INV4', 100),
        ('B', 'INV5', 150)
    ) AS t(customer, invoice, amount)
),

payments AS (
    SELECT * FROM (VALUES
        ('A', 220),
        ('B', 150)
    ) AS t(customer, payment)
),

-- Kluczowa część: suma narastająca faktur (odpowiednik accumulate)
-- running_total = ile łącznie "zużyto" miejsca na faktury do tej pory (włącznie z bieżącą)
-- prev_total    = ile łącznie "zużyto" przed bieżącą fakturą
running AS (
    SELECT
        i.customer,
        i.invoice,
        i.amount,
        i.rn,
        p.payment,
        SUM(i.amount) OVER (PARTITION BY i.customer ORDER BY i.rn) AS running_total,
        COALESCE(
            SUM(i.amount) OVER (PARTITION BY i.customer ORDER BY i.rn
                                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING),
            0
        ) AS prev_total
    FROM invoices i
    JOIN payments p ON i.customer = p.customer
)

SELECT
    customer,
    invoice,
    amount,
    -- Ile z płatności "dociera" do tej faktury:
    -- GREATEST(..., 0) zabezpiecza przed ujemnymi wartościami
    GREATEST(LEAST(payment, running_total) - prev_total, 0) AS allocated,
    amount - GREATEST(LEAST(payment, running_total) - prev_total, 0) AS remaining
FROM running
ORDER BY customer, rn;