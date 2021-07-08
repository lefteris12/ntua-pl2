<?php
session_start();
if (!isset($_SESSION["question"])) {
    $_SESSION["question"] = 0;
}
if (!isset($_SESSION["time_started"])) {
    $_SESSION["time_started"] = microtime(true);
}
$q = $_SESSION["question"];
?>

<!DOCTYPE html>
<html>
<body>

<?php
function encode_char($char) {
    $morse_encode = array(
        "a"=>"·–",
        "b"=>"–···", 
        "c"=>"–·–·", 
        "d"=>"–··", 
        "e"=>"·",
        "f"=>"··–·", 
        "g"=>"––·", 
        "h"=>"····", 
        "i"=>"··", 
        "j"=>"·–––", 
        "k"=>"–·–", 
        "l"=>"·–··", 
        "m"=>"––", 
        "n"=>"–·", 
        "o"=>"–––", 
        "p"=>"·––·", 
        "q"=>"––·–", 
        "r"=>"·–·", 
        "s"=>"···", 
        "t"=>"–", 
        "u"=>"··–", 
        "v"=>"···–", 
        "w"=>"·––", 
        "x"=>"–··–", 
        "y"=>"–·––", 
        "z"=>"––··",
        " "=>""
    );
    return $morse_encode[$char];
} 


$questions = array(
    "hello world",
    "oh my god it is unicode",
    "do not panic",
    "more instructions in the answers below",
    "i hope you are having fun with morse code",
    "did you notice the two spaces between words in html source question mark",
    "morse code is a method of transmitting text information as a series of on off tones lights or clicks that can be directly understood by a skilled listener or observer without special equipment",
    "here are the rules for this game stop your text should contain just letters and spaces stop lowercase and uppercase are considered the same stop in your morse code you should have one space between letters and two spaces between words stop have fun stop over and out",
    "i may not have gone where i intended to go but i think i have ended up where i needed to be",
    "in the beginning the universe was created stop this has made a lot of people very angry and been widely regarded as a bad move stop",
    "there is a theory which states that if ever anyone discovers exactly what the universe is for and why it is here it will instantly disappear and be replaced by something even more bizarre and inexplicable stop there is another theory which states that this has already happened",
    "it is known that there are an infinite number of worlds simply because there is an infinite amount of space for them to be in stop however not every one of them is inhabited stop therefore there must be a finite number of inhabited worlds stop any finite number divided by infinity is as near to nothing as makes no odds so the average population of all the planets in the universe can be said to be zero stop from this it follows that the population of the whole universe is also zero and that any people you may meet from time to time are merely the products of a deranged imagination"
);

// Text to morse
function encode($text) {
    $chars = array_map('encode_char', str_split($text));
    echo implode(' ', $chars);
}
?>

<h1>Yet another simple code game!</h1>

<p><span class="question">Question <?php echo $q+1 ?></span>:</p>
<code><?php encode($questions[$q]) ?></code>
<br>
<span class="question">Make it quick, the clock is ticking...</span>

<?php if (!isset($_POST['answer'])) { ?>
<form action="<?php echo $_SERVER['PHP_SELF'];?>" id="f" name="f" method="post">
  <textarea class="wide" name="answer" id="answer"></textarea><br />
  <input type="submit" name="submit" id="submit" value="Submit!" />
  <input type="reset" value="Reset" />
</form>
<?php } ?>

<?php
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $value = 'Continue!';
    if (isset($_POST['answer'])) {
        $answer = trim($_POST['answer']);
        unset($_POST['answer']);
        if ($answer == $questions[$q]) { 
        ?>
            <h3>RIGHT!!!</h3>
        <?php
            if ($_SESSION["question"] == count($questions) - 1) {
                $_SESSION["question"] = 0;
                $value = 'Play again!';
        ?>
            <p>It took you <?php echo microtime(true) - $_SESSION["time_started"] ?> seconds.</p>
        <?php
            } else {
                $_SESSION["question"]++;
            }
        }
        else { ?>
            <h3>WRONG!!!</h3>
        <?php } ?>
            <hr />
            <form action="<?php echo $_SERVER['PHP_SELF'];?>" id="r" name="r" method="post">
            <input type="hidden" id="continue" name="continue" value="continue" />
            <input type="submit" name="again" id="again" value="<?php echo $value ?>" />
            </form>
        <?php
    }
}
?>

</body>
</html>
