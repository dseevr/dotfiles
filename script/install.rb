#!/usr/bin/env ruby

require "fileutils"

BASE_DIR = File.expand_path("../..", __FILE__)
HOME_DIR = ENV["HOME"]


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

# TODO: directory-based linking
