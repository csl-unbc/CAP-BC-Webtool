var $slider = $(".slideshow .slider"),
  maxItems = $(".item", $slider).length,
  dragging = false,
  tracking;

$slider.addClass("slideshow-left");
$(".slideshow-left")
  .slick({
    vertical: true,
    verticalSwiping: true,
    arrows: false,
    infinite: false,
    dots: true,
    speed: 1000,
    cssEase: "cubic-bezier(0.7, 0, 0.3, 1)",
    customPaging : function(slider, i) {
      var title = $(slider.$slides[i]).find('.item').attr('title');
      return '<a class="item_dot"> '+title+' </a>';
    }
  })
  .on("beforeChange", function (event, slick, currentSlide, nextSlide) {
    if (
      currentSlide > nextSlide &&
      nextSlide == 0 &&
      currentSlide == maxItems - 1
    ) {
      $(".slideshow-text").slick("slickGoTo", maxItems);
    } else if (
      currentSlide < nextSlide &&
      currentSlide == 0 &&
      nextSlide == maxItems - 1
    ) {
      $(".slideshow-text").slick("slickGoTo", -1);
    } else {
      $(".slideshow-text").slick("slickGoTo", nextSlide);
    }
  })
  .on("mousewheel", function (event) {
    event.preventDefault();
    if (event.deltaX > 0 || event.deltaY < 0) {
      $(this).slick("slickNext");
    } else if (event.deltaX < 0 || event.deltaY > 0) {
      $(this).slick("slickPrev");
    }
  })
  .on("mousedown touchstart", function () {
    dragging = true;
    tracking = $(".slick-track", $slider).css("transform");
    tracking = parseInt(tracking.split(",")[5]);
  })
  .on("mousemove touchmove", function () {
    if (dragging) {
      newTracking = $(".slideshow-left .slick-track").css("transform");
      newTracking = parseInt(newTracking.split(",")[5]);
      diffTracking = newTracking - tracking;
    }
  })
  .on("mouseleave touchend mouseup", function () {
    dragging = false;
  });


$(".slideshow-text").slick({
  swipe: false,
  vertical: true,
  arrows: false,
  infinite: true,
  speed: 1000,
  cssEase: "cubic-bezier(0.7, 0, 0.3, 1)",
});

