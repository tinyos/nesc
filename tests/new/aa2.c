#line 1 "tos.h"
typedef struct TOS_Msg {

  short addr;
  char type;
  unsigned char group;
  char data[30];
  short crc;
  short strength;
} TOS_Msg;

typedef TOS_Msg *TOS_MsgPtr;

unsigned short TOS_LOCAL_ADDRESS;

enum  {
  TOS_BCAST_ADDR = (short )0xffff
} ;
 char counter$output$output_complete(char arg_0x82b5070);
 char counter$init$start(void);
 char counter$init$init(void);
 void counter$clock$fire(void);
 void int_to_leds_m$input$output(int arg_0x82b4c28);
 void int_to_leds_m$input$init(void);
 char leds$leds$yellow_off(void);
 char leds$leds$red_on(void);
 char leds$leds$green_on(void);
 char leds$leds$red_off(void);
 char leds$leds$yellow_on(void);
 char leds$leds$init(void);
 char leds$leds$red_toggle(void);
 char leds$leds$green_toggle(void);
 char leds$leds$yellow_toggle(void);
 char leds$leds$green_off(void);
 void int_to_rfm_m$input$output(int arg_0x82b4c28);
 void int_to_rfm_m$input$init(void);
 char int_to_rfm_m$send$send_done(struct TOS_Msg *arg_0x82e5a10);
 char generic_comm$send_msg$default$send_done(unsigned char arg_0x82f01a8, struct TOS_Msg *arg_0x82e5a10);
 char generic_comm$send_msg$send(unsigned char arg_0x82f01a8, short arg_0x82e5390, struct TOS_Msg *arg_0x82e5558);
 void clock$clock$set_rate(char arg_0x82b59f8, char arg_0x82b5b58);
inline  char main$simple_init$start(void){
  char result;

  result = counter$init$start();

  return result;
}
inline  char main$simple_init$init(void){
  char result;

  result = counter$init$init();

  return result;
}
#line 6 "main.td"
int main$x;
inline  void counter$output$output(int arg_0x82b4c28){
  int_to_leds_m$input$output(arg_0x82b4c28);
  int_to_rfm_m$input$output(arg_0x82b4c28);
}
inline  void counter$output$init(void){
  int_to_leds_m$input$init();
  int_to_rfm_m$input$init();
}
inline  void counter$clock$set_rate(char arg_0x82b59f8, char arg_0x82b5b58){
  clock$clock$set_rate(arg_0x82b59f8, arg_0x82b5b58);
}
#line 11 "counter.td"
int counter$state;
 
char counter$init$init(void)
{
  counter$state = 0;
  counter$output$init();
  return 1;
}

 char counter$init$start(void)
{
  counter$clock$set_rate(1, 2);
  return 1;
}

 void counter$clock$fire(void)
{
  counter$state++;
  counter$output$output(counter$state);
}

 char counter$output$output_complete(char success)
{
  return 1;
}

inline  char int_to_leds_m$input$output_complete(char arg_0x82b5070){
  char result;

  result = counter$output$output_complete(arg_0x82b5070);

  return result;
}
inline  char int_to_leds_m$leds$yellow_off(void){
  char result;

  result = leds$leds$yellow_off();

  return result;
}
inline  char int_to_leds_m$leds$red_on(void){
  char result;

  result = leds$leds$red_on();

  return result;
}
inline  char int_to_leds_m$leds$green_on(void){
  char result;

  result = leds$leds$green_on();

  return result;
}
inline  char int_to_leds_m$leds$red_off(void){
  char result;

  result = leds$leds$red_off();

  return result;
}
inline  char int_to_leds_m$leds$yellow_on(void){
  char result;

  result = leds$leds$yellow_on();

  return result;
}
inline  char int_to_leds_m$leds$init(void){
  char result;

  result = leds$leds$init();

  return result;
}
inline  char int_to_leds_m$leds$red_toggle(void){
  char result;

  result = leds$leds$red_toggle();

  return result;
}
inline  char int_to_leds_m$leds$green_toggle(void){
  char result;

  result = leds$leds$green_toggle();

  return result;
}
inline  char int_to_leds_m$leds$yellow_toggle(void){
  char result;

  result = leds$leds$yellow_toggle();

  return result;
}
inline  char int_to_leds_m$leds$green_off(void){
  char result;

  result = leds$leds$green_off();

  return result;
}
 
#line 9 "int_to_leds_m.td"
void int_to_leds_m$input$init(void)
{
  int_to_leds_m$leds$init();
  int_to_leds_m$leds$red_off();
  int_to_leds_m$leds$yellow_off();
  int_to_leds_m$leds$green_off();
}

 void int_to_leds_m$output_done(void)
{
  int_to_leds_m$input$output_complete(1);
}

 void int_to_leds_m$input$output(int value)
{
  if (value & 1) {
#line 24
    int_to_leds_m$leds$yellow_on();
    }
  else {
#line 25
    int_to_leds_m$leds$yellow_off();
    }
#line 26
  if (value & 2) {
#line 26
    int_to_leds_m$leds$green_on();
    }
  else {
#line 27
    int_to_leds_m$leds$green_off();
    }
#line 28
  if (value & 4) {
#line 28
    int_to_leds_m$leds$red_on();
    }
  else {
#line 29
    int_to_leds_m$leds$red_off();
    }
  TOS_post(int_to_leds_m$output_done);
}

 
#line 6 "leds.td"
char leds$leds$init(void)
#line 6
{
}

 
#line 7
char leds$leds$red_on(void)
#line 7
{
}

 
#line 8
char leds$leds$red_off(void)
#line 8
{
}

 
#line 9
char leds$leds$red_toggle(void)
#line 9
{
}

 
#line 10
char leds$leds$green_on(void)
#line 10
{
}

 
#line 11
char leds$leds$green_off(void)
#line 11
{
}

 
#line 12
char leds$leds$green_toggle(void)
#line 12
{
}

 
#line 13
char leds$leds$yellow_on(void)
#line 13
{
}

 
#line 14
char leds$leds$yellow_off(void)
#line 14
{
}

 
#line 15
char leds$leds$yellow_toggle(void)
#line 15
{
}

inline  char int_to_rfm_m$input$output_complete(char arg_0x82b5070){
  char result;

  result = counter$output$output_complete(arg_0x82b5070);

  return result;
}
inline  char int_to_rfm_m$send$send(short arg_0x82e5390, struct TOS_Msg *arg_0x82e5558){
  char result;

  result = generic_comm$send_msg$send(4, arg_0x82e5390, arg_0x82e5558);

  return result;
}
#line 10 "int_to_rfm_m.td"
typedef struct int_to_rfm_m$mymsg {
  char val;
  int src;
} int_to_rfm_m$int_to_led_msg;

char int_to_rfm_m$pending;
struct TOS_Msg int_to_rfm_m$data;
 
void int_to_rfm_m$input$init(void)
{
}

 void int_to_rfm_m$input$output(int value)
{
  int_to_rfm_m$int_to_led_msg *message = (int_to_rfm_m$int_to_led_msg *)int_to_rfm_m$data.data;

  if (!int_to_rfm_m$pending) 
    {
      int_to_rfm_m$pending = 1;
      message->val = value;
      message->src = TOS_LOCAL_ADDRESS;
      if (!int_to_rfm_m$send$send(TOS_BCAST_ADDR, &int_to_rfm_m$data)) {
        int_to_rfm_m$pending = 0;
        }
    }
}

 
#line 36
char int_to_rfm_m$send$send_done(struct TOS_Msg *msg)
{
  if (int_to_rfm_m$pending && msg == &int_to_rfm_m$data) 
    {
      int_to_rfm_m$pending = 0;
      int_to_rfm_m$input$output_complete(1);
    }
}

inline  char generic_comm$send_msg$send_done(unsigned char arg_0x82f01a8, struct TOS_Msg *arg_0x82e5a10){
  char result;

  switch (arg_0x82f1478) {
    case 4:
      result = int_to_rfm_m$send$send_done(arg_0x82f1640);
      break;
    default:
      result = generic_comm$send_msg$default$send_done(arg_0x82f01a8, arg_0x82f1640);
    }

  return result;
}
  
#line 10 "generic_comm.td"
char generic_comm$send_msg$send_done(
#line 4
unsigned char id, 





struct TOS_Msg *msg)
{
  return 1;
}

 char generic_comm$send_msg$send(
#line 4
unsigned char id, 
#line 15
short address, struct TOS_Msg *msg)
{
  generic_comm$send_msg$send_done(id, msg);
  return 0;
}

inline  void clock$clock$fire(void){
  counter$clock$fire();
}
 
#line 6 "clock.td"
void clock$clock$set_rate(char interval, char scale)
#line 6
{
}

