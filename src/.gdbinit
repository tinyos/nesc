define pname
  if $arg0->container
    printf "%s$", $arg0->container->name
  end
  if $arg0->interface
    printf "%s$", $arg0->interface->name
  end
  printf "%s\n", $arg0->name
end
