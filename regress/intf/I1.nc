interface I1
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
command result_t init();

}

