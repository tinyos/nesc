generic configuration TemInst() {
 provides interface Tem<int>;
}
implementation {
 components new TemInst();
 Tem = TemInst.Tem;
}
