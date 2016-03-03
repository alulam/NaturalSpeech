using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Speech.Synthesis;
using System.IO;

namespace SoundGeneration
{
    class Program
    {
        static void Main(string[] args)
        {
            foreach (string word in args)
            {
                using (FileStream f = new FileStream(word + ".wav", FileMode.Create))
                {
                    using (SpeechSynthesizer speak = new SpeechSynthesizer())
                    {
                        speak.SetOutputToWaveStream(f);
                        speak.Speak(word);
                    }
                }
            }
        }
    }
}
