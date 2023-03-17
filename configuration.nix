# /etc/nixos/configuration.nix                              -*-nix-*-

# 1. This file was bootstrapped via `nixos-generate-config --root /mnt`
# 2. Copy into /etc/nixos/ which will likely be under /mnt during install
# 3. First time sys-gen: nixos-install
# 4. Updates to running system: sudo nixos-rebuild switch
# 5. Prune occasionally: nix-collect-garbage -d


# FIXME: compile a Rust program with: [dependencies] openssl-sys="0.9"
# https://nixos.wiki/wiki/FAQ/Libraries
# https://nixos.org/nixpkgs/manual/#chap-multiple-output
# Hint: `nix-shell -p openssl`

# TODO:
# - cryptsetup
# - wired ethernet via USB dongle; might be an issue with kernels newer than 5.3

# See https://nixos.org/nixos/manual/ or configuration.nix(5) man page
# or when running locally, run ‘nixos-help’ which opens the web browser
# to a `file:` URL.

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      # FIXME: edit hardware-configuration.nix due to bogus UUID for /boot
      # and overriding via fileSystems."/boot".device trigger a conflict.
      # Confirm via `lsblk -o name,uuid,mountpoint`
      ./hardware-configuration.nix
    ];

  # For less wear & tear on SSD, add options: noatime,discard
  fileSystems."/".options = [ "noatime" "discard" "errors=remount-ro" ];
  fileSystems."/boot".options = [ "noatime" "umask=0077" ];
  fileSystems."/home" = {
    device = "/dev/sda4";
    fsType = "ext4";
    options = [ "noatime" "discard" ];
  };
  # https://nixos.org/nixos/manual/options.html#opt-swapDevices
  # Being Linux and not BSD Unix, using swap indicates overload, so keep it small
  swapDevices = [
    {
      device = "/swapfile";
      size = 1024;
    }
  ];

  # The systemd-boot EFI boot loader is unable to load EFI binaries
  # from other partitions, so use Grub2 instead for dual-boot.
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
    useOSProber = true; # Append entries for other OSs detected by os-probe
    devices = [ "nodev" ]; # Must be set for an ASSERT to pass in grub.nix
    efiSupport = true;
    enable = true;
  };

  # Just in case:
  # Default kernel in 20.03 is 5.4 stable; 5.6 is stable; 4.19 is prev LTS.
  #boot.kernelPackages = pkgs.linuxPackages_latest;
  #boot.kernelPackages = pkgs.linuxPackages_5_6;
  #boot.kernelPackages = pkgs.linuxPackages_4_19;

  boot.cleanTmpDir = true;

  networking.hostName = "nixos"; # FIXME: Define your hostname.

  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # For wifi SSID, see /etc/wpa_supplicant.conf

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;
  # This Ethernet interface attaches via USB dongle, and when disconnected
  # will produce log entry, "Timed out waiting for device ..."
  #FIXME: interface cycles between up/down state with kernel 5.4, 5.5
  #networking.interfaces.enp0s20u1.useDHCP = true; # "renamed from eth0"

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "US/Mountain";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    emacs vim firefox thunderbird wmctrl jumpapp
    xfce.xfdesktop xfce.xfce4-battery-plugin xfce.xfce4-screenshooter
    xfce.xfce4-pulseaudio-plugin xfce.xfce4-volumed-pulse
    xfce.ristretto xorg.oclock
    #networkmanager networkmanagerapplet
    plank #xfce.xfce4-panel
    dict dictdDBs.wordnet wordnet xfce.xfce4-dict dictdDBs.wiktionary
    git ripgrep ripgrep-all xsv
    clang gnumake pandoc binutils pkg-config libressl zlib

    # Packages for Rust and Common-Lisp work fine for convention use:
    # rPackages.rust rls rustracer rustfmt rustup cargo cargo-outdated clippy diesel-cli
    # sbcl rPackages.hyperSpec lispPackages.swank lispPackages.quicklisp
    # As critical path tools, however, those will be installed
    # directly from official upstream distributions channels.
    # HOWEVER, official `rustup` won't install as-is, so need it from pkgs:
    rustup

    python3 #python3Full
    wget whois traceroute
    mpd mpc_cli python38Packages.notify2 #xfce.xfce4-mpc-plugin
    # audacity blender inkscape imagemagick
    docker vagrant virtualbox

    # These require .allowUnfree=true:
    google-chrome teams freeoffice microcodeIntel #microcodeAmd
  ];
  nixpkgs.config.allowUnfree = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedTCPPortRanges = [ { from = 8080; to = 8089 } ];
  networking.firewall.enable = true;
  networking.firewall.allowPing = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    # https://nixos.wiki/wiki/Xfce
    displayManager.defaultSession = "xfce";
    desktopManager = {
      xterm.enable = false;
      xfce.enable = true;
    };

    # Enable touchpad support.
    libinput.enable = true;
    # xkbOptions = "eurosign:e";
  };

  environment.variables = {
    # Play nice with dual-boot Linux (e.g., Xubuntu) where PATHs
    # differ (such as for autostart, plank, jumpapp), because NixOS
    # doesn't abide by Linux FHS policies.  Maybe begin with:
    # `rsync -a ~/.config/ ~/.config_nixos/`
    # However, avoid hard-links or sym-links, in case any particular
    # app modifies a config file in-place.
    #
    # Then, convert keyboard shortcut settings to use NixOS paths:
    # sed 's%/usr/bin/%/run/current-system/sw/bin/%g' \
    #  < ~/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml
    #  > ~/.config_nixos/xfce4/xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml
    #
    # Perform similar changes for each Autostart and Plank Launcher files:
    # ~/.config/autostart/*.desktop, ~/.config/plank/dock*/launchers/*.dockitem
    XDG_CONFIG_HOME = "$HOME/.config_nixos";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.daniel = {
    description = "Daniel";
    isNormalUser = true;
    extraGroups = [ "wheel" "staff" "docker" ];
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03";
}
