<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>I Love You Heena</title>
    <style>
        body {
            background-color: black;
            color: white;
            text-align: center;
            font-family: Arial, sans-serif;
            height: 100vh;
            display: flex;
            justify-content: center;
            align-items: center;
            flex-direction: column;
        }
        h1 {
            font-size: 50px;
            animation: glow 1.5s infinite alternate;
        }
        @keyframes glow {
            from { text-shadow: 0 0 10px pink, 0 0 20px red; }
            to { text-shadow: 0 0 20px pink, 0 0 30px red; }
        }
    </style>
</head>
<body>

    <h1>I Love You Heena ❤️</h1>
    
    <!-- Romantic Music -->
    <audio autoplay loop>
        <source src="romantic-music.mp3" type="audio/mp3">
        Your browser does not support the audio element.
    </audio>

</body>
</html>
