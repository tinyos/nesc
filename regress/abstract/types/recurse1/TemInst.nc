generic configuration TemOOPS() {
 provides interface Tem<int>;
}
implementation {
 components new TemOOPS();
 Tem = TemOOPS.Tem;
}
