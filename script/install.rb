#!/usr/bin/env ruby

require "fileutils"

BASE_DIR = File.expand_path("../..", __FILE__)
HOME_DIR = ENV["HOME"]

# TODO: replace all of this code with a simple array of [source, dest] pairs
#       if dest has slashes in it, run FileUtils.mkdir_p first

# Simple dotfiles can be symlinked into $HOME
Dir.glob("#{BASE_DIR}/.*").each do |source_path|
  filename = source_path.split("/").last

  # skip current, parent, git directory, and .gitignore file
  next if %w[. .. .git .gitignore].include?(filename)

  target_path = "#{HOME_DIR}/#{filename}"

  if File.exist?(target_path)
    puts "WARNING: Skipping #{filename} because ~/#{filename} already exists"
    next
  end

  FileUtils.ln_s(source_path, target_path)

  puts "Linking #{filename} -> ~/#{filename}"
end

# Configs with custom paths

# htoprc
htop_src = "#{BASE_DIR}/htoprc"
htop_dir = "#{HOME_DIR}/.config/htop"
htop_dest = htop_dir + "/htoprc"

if File.exist?(htop_dest)
  puts "WARNING: skipping htoprc because #{htop_dest} already exists"
else
  puts "Linking #{htop_src} -> #{htop_dest}"
  FileUtils.mkdir_p(htop_dir)
  FileUtils.ln_s(htop_src, htop_dest)
end

