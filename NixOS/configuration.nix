{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
boot.kernelParams = ["usbcore.autosuspend=-1 iommu=pt "];
  # Use the systemd-boot EFI boot loader.
  hardware.cpu.intel.updateMicrocode = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Asia/Kolkata";
  # nixUnstable
  nix.package = pkgs.nixUnstable;
package = pkgs.nixFlakes;
 extraOptions = lib.optionalString (config.nix.package == pkgs.nixFlakes)
     "experimental-features = nix-command flakes";
  # non-free stuff
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;
    nix.autoOptimiseStore = true;
  networking.useDHCP = false;
  networking.interfaces.enp0s3.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];

  # Enable X11 for EXWM.
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.videoDrivers = [ "modesetting" ];
  # Configure keymap in X11
  services.xserver.layout = "us";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
  };
  services.pipewire  = {
    media-session.config.bluez-monitor.rules = [
      {
        # Matches all cards
        matches = [ { "device.name" = "~bluez_card.*"; } ];
        actions = {
          "update-props" = {
            "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
            # mSBC is not expected to work on all headset + adapter combinations.
            "bluez5.msbc-support" = true;
            # SBC-XQ is not expected to work on all headset + adapter combinations.
            "bluez5.sbc-xq-support" = true;
          };
        };
      }
      {
        matches = [
          # Matches all sources
          { "node.name" = "~bluez_input.*"; }
          # Matches all outputs
          { "node.name" = "~bluez_output.*"; }
        ];
        actions = {
          "node.pause-on-idle" = false;
        };
      }
    ];
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ak = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  environment.systemPackages = with pkgs; [
    wget
    vim
    alacritty
    xorg.xkill
    git
    man
    kitty
    lxappearance
    lxsession
    libnotify
    xclip
    starship
    cmake
    volumeicon
    usbutils
    pavucontrol
    killall
    htop
    firefox
    chromium
    neofetch
    discord
    picom
    hack-font
    xarchiver
    unzip
    nitrogen
    rofi
    trayer
    arc-theme
    xfce.xfce4-clipman-plugin
    youtube-dl
    xfce.xfce4-notifyd
    mpv
    qbittorrent
    mesa-demos
    glxinfo
    xorg.xdpyinfo
    brightnessctl
    imagemagick
    exa
    gcc
    xorg.xmodmap
    gimp
    brave
    bpytop
    bat
    lolcat
    ncdu
    lm_sensors
    rnix-lsp
    tmux
    ps_mem
    noto-fonts
    ntfs3g
    gparted
    file
    cachix
    feh
    speedtest-cli
    gnumake
    clang-tools
    ed
    autoconf
    automake
    inkscape
    gdk-pixbuf
    sassc
    pkgconfig
    rustup
    # python stuff
    python39
    # Install Emacs With VTerm
    ((emacsPackagesNgGen emacsPgtkGcc).emacsWithPackages
      (epkgs: [
        epkgs.vterm]))
  ];
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  networking.firewall.enable = false;

  system.stateVersion = "21.05";
