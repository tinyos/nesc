define pname
  if $arg0->container
    printf "%s$", $arg0->container->name
  end
  if $arg0->interface
    printf "%s$", $arg0->interface->name
  end
  printf "%s\n", $arg0->name
end

define regress
  cd ../nregress/$arg0
end

define app
  cd /home/dgay/motes/tinyos-1.x/apps/$arg0
end

define app2
  cd /home/dgay/motes/2.x/apps/$arg0
end

define mrun
  run -_fnesc-include=tos -_fnesc-target=avr -_fnesc-no-debug -mmcu=atmega103 -DPLATFORM_MICA -DNESC=110 -I\/home/dgay/motes/tinyos-1.x/tos/platform/mica -I\/home/dgay/motes/tinyos-1.x/tos/platform/avrmote -I\/home/dgay/motes/tinyos-1.x/tos/interfaces -I\/home/dgay/motes/tinyos-1.x/tos/types -I\/home/dgay/motes/tinyos-1.x/tos/system -v $arg0
end
