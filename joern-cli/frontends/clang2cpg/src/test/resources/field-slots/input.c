struct RenamedClock { long ticks; unsigned :3; long nanos; int :0; };
struct PaddingWidths { unsigned :1; int kept; unsigned :7; };
struct CarrierBox { unsigned flag:2; struct { int payload; }; };
long renamed_sample(void) {
  struct RenamedClock moment = {.nanos=29, .ticks=11};
  struct PaddingWidths width = {.kept=3};
  struct CarrierBox carrier = {.payload=7, .flag=2};
  return moment.ticks + moment.nanos + width.kept + carrier.payload + carrier.flag;
}
int main(void) { return renamed_sample() != 52; }
