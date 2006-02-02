module atomicfun {
}
implementation {
  void check1() {
    atomic {
      return; // no
    }
  }

  int check2() {
    atomic {
      return 1; // no
    }
    return 0;
  }

  void check3() {
    atomic {
    x: goto x; // yes
    }
  }

  void check4() {
    goto x;
    atomic {
    x: ; // no
    }
  }

  void check5() {
    atomic {
    goto x; // no
    }
  x: ;
  }

  void check6() {
    atomic {
      atomic {
	goto x; // no
      }
    x: ;
    }
  }

  void check7() {
    atomic {
      switch (1) // yes
	{
	case 2:
	  break;
	default:
	  break;
	}
    }
  }

  void check8() {
    switch (1) // no
      atomic {
      case 2:
	break;
      default:
	break;
    }
  }

  void check9() {
    switch (1)
      {
	atomic {
	case 2: // no
	  break; // no
	}
      default: // yes
	break; // yes
    }
  }

  void check10() {
    atomic {
      while (1)
	break; // ok
    }
  }

  void check11() {
    while (1)
      atomic break; // no
  }
}
