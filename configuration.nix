# /etc/nixos/configuration.nix                              -*-conf-*-

# 1. This file was bootstrapped via `nixos-generate-config --root /mnt`
# 2. Copy into /etc/nixos/ which will likely be under /mnt during install
# 3. First time sys-gen: nixos-install
# 4. Updates to running system: sudo nixos-rebuild switch
# 5. Prune occasionally: nix-collect-garbage -d

# See https://nixos.org/nixos/manual/ or configuration.nix(5) man page
# or when running locally, run ‘nixos-help’.

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
  # Default kernel in 20.03 is 5.4 stable; 5.5 is EOL stable; 4.19 is prev LTS.
  #boot.kernelPackages = pkgs.linuxPackages_latest;
  #boot.kernelPackages = nixos.linuxPackages_5_5.kernel;
  #boot.kernelPackages = nixos.linuxPackages_4_19.kernel;

  networking.hostName = "nixos"; # FIXME: Define your hostname.

  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # For wifi SSID, see /etc/wpa_supplicant.conf

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };
  i18n.defaultLocale = "en_CA.UTF-8";

  # Set your time zone.
  time.timeZone = "Canada/Pacific";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    emacs vim firefox thunderbird wmctrl jumpapp
    xfce.xfdesktop xfce.xfce4-battery-plugin
    xfce.ristretto xorg.oclock
    dict dictdDBs.wordnet wordnet xfce.xfce4-dict dictdDBs.wiktionary
    git ripgrep ripgrep-all xsv
    gnumake pandoc
    rPackages.rust rls rustracer rustfmt rustup cargo cargo-outdated clippy diesel-cli
    sbcl rPackages.hyperSpec lispPackages.swank lispPackages.quicklisp
    python3 #python3Full
    wget whois traceroute
    mpd mpc_cli python38Packages.notify2 #xfce.xfce4-mpc-plugin
    # audacity blender inkscape imagemagick
    docker vagrant virtualbox
    # These require .allowUnfree=true:
    teams google-chrome
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
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.daniel = {
    description = "Daniel";
    isNormalUser = true;
    extraGroups = [ "wheel" "staff" "docker" ]; # Enable ‘sudo’ for the user.
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03";
}
