
interface MathOps<type>
{
  async command type cast_to( uint64_t a );
  async command uint64_t cast_from( type a );
  async command type inc( type a );
  async command type dec( type a );
  async command type add( type a, type b );
  async command type sub( type a, type b );
  async command type not( type a );
  async command type and( type a, type b );
  async command type or( type a, type b );
  async command type xor( type a, type b );
  async command type sl( type a, uint8_t n );
  async command type sr( type a, uint8_t n );
  async command bool eq( type a, type b );
  async command bool ne( type a, type b );
  async command bool lt( type a, type b );
  async command bool gt( type a, type b );
  async command bool le( type a, type b );
  async command bool ge( type a, type b );
}

