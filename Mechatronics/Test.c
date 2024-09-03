// Pin connected to the LED
const int ledPin = 9;

void setup() {
    // Set the LED pin as an output
    pinMode(ledPin, OUTPUT);
}

void loop() {
    // Generate a PWM signal with a duty cycle of 50%
    analogWrite(ledPin, 128);
    
    // Wait for 1 second
    delay(1000);
    
    // Generate a PWM signal with a duty cycle of 25%
    analogWrite(ledPin, 64);
    
    // Wait for 1 second
    delay(1000);
}