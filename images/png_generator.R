library(fontawesome)
# thumbs-up
# headphones
# scale-balanced
# person-breastfeeding
# microphone
# computer
# check
# person-chalkboard
# forestgreen
# envelope-circle-check
# person-running
# mug-saucer
# bullhorn
# house-user
# person-chalkboard
# volume-xmark
# calendar-check
# calendar-xmark
# calendar-plus
# find the icon on https://fontawesome.com/icons, then save to computer 
# using the name of the icon. no need to download svg and convert

if (interactive()) {
  # Create a Font Awesome SVG icon as a
  # PNG file on disk
  
  fa_png(name = "envelope-circle-check",
         file="./class_materials/syllabus/course_schedule/icons/envelope-circle-check.png",
         fill="white")
  
}