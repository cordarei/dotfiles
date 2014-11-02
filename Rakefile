require 'rake'


def herefile name
  return "#{pwd}/#{name}"
end

def homefile name
  return "#{ENV['HOME']}/.#{name}"
end

def configfile name
  return "#{ENV['HOME']}/.config/#{name}"
end

def localfile name
  return "#{ENV['HOME']}/.local/#{name}"
end



def dolink filename, linkname
  puts "# symlinking '#{filename}' to '#{linkname}' ..."
  if File.exist? linkname
    puts "# '#{linkname}' exists!"
    if File.symlink? linkname
      puts "# '#{linkname}' is a symlink! Removing ..."
      rm linkname
    else
      if File.directory? filename and File.directory? linkname
        puts "# '#{linkname}' is a directory! Moving '#{linkname}' to '#{linkname}.bak' ..."
        mv linkname, "#{linkname}.bak"
      end
    end
  end
  ln_sf filename, linkname, :verbose => true
end


def doconfig name
  s = herefile("config/#{name}")
  t = configfile(name)
  dolink s, t
end

def dohome name
  s = herefile(name)
  t = homefile("#{name}")
  dolink s,t
end


task :git do
  doconfig 'git'
end

task :fish do
  doconfig 'fish'
end

task :awesome do
  doconfig 'awesome'
end

task :emacs do
  dohome 'emacs.d'
end

task :tmux do
  dohome 'tmux.conf'
end

task :vim do
  dohome 'vim'
  dohome 'vimrc'
end

task :localbin do
    FileList['local/bin/*'].each do |f|
        s = herefile(f)
        t = localfile('bin/')
        dolink s,t
    end
end


task :default => [:git, :fish, :awesome, :emacs, :tmux, :vim, :localbin]
