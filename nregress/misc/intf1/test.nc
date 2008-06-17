interface test
{
/***/
enum {
MOTOR_FORWARD = 1,
MOTOR_REVERSE = 0,
MOTOR_OFF = 0
};
/***/

/**
* Init.
* @return Always returns SUCCESS.
*/
command void init();

}

