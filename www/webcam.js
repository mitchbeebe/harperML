// Initialize webcam and image settings
const constraints = {
  video: true
};

const captureVideoButton = document.getElementById('capture-button');
const screenshotButton = document.getElementById('screenshot-button');
const img = document.getElementById('screenshot-img');
const video = document.querySelector('video');
const canvas = document.createElement('canvas');

// When the video button is clicked, open the video webcam and begin streaming
captureVideoButton.onclick = function() {
  navigator.mediaDevices.getUserMedia(constraints).
    then(handleSuccess).catch(handleError);
};

// When the screenshot button or the video itself is clicked,
// grab a still image of the stream and replace the blank canvas
screenshotButton.onclick = video.onclick = function() {
  canvas.width = video.videoWidth;
  canvas.height = video.videoHeight;
  canvas.getContext('2d').drawImage(video, 0, 0);
  img.src = canvas.toDataURL('image/png');
  Shiny.setInputValue("img_src", img.src);
  Shiny.setInputValue("screenshot", 1, {priority: "event"});
};

function handleSuccess(stream) {
  screenshotButton.disabled = false;
  video.srcObject = stream;
}

function handleError(error) {
  alert("Unable to access camera!");
}

