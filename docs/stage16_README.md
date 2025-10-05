# Stage-16: Metrics & Reflective Gain (Local Only)

این راهنما نحوه اجرای **مرحله ۱۶** به‌صورت کاملاً لوکال (بدون GitHub) را توضیح می‌دهد.

## آن‌چه می‌سازد
با اجرای پایپ‌لاین، این فایل‌ها در `build/metrics/` تولید می‌شوند:
- `summary_stage16.json` — خلاصه متریک‌ها به تفکیک `pre`/`post` و Δ
- `summary_stage16.csv` — نسخه جدولی برای گزارش/اکسل
- `stage16_table.tex` — جدول لاتک متریک‌های تصمیم/Trace (Acc, F1, TraceC, …)
- `stage16_table_extra.tex` — جدول لاتک Coverage/Hallucination/Runtime
- `stage16_table_latency.tex` — جدول لاتک Latency (retrieve/nlq/reason)
- `stage16_tables.tex` — تجمیع هر سه جدول برای `\input{}` در مقاله
- `stage16_validation.txt` — گزارش ولیدیشن رکوردها

## اجرای سریع (Windows)
از ریشهٔ ریپو:
```bat
.\stage16_run_all.bat
