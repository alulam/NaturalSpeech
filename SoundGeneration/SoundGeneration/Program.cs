﻿using System;
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
        static void Main()
        {

            IList<string> args = new List<string>();
            string s = Console.ReadLine();
            while(!string.IsNullOrEmpty(s)){
                args.Add(s);
                s = Console.ReadLine();
            }

            foreach (string word in args)
            //Create wav file for each sound
            {
                using (FileStream f = new FileStream(word + ".wav", FileMode.Create))
                {
                    using (SpeechSynthesizer speak = new SpeechSynthesizer())
                    {
                        speak.SetOutputToWaveStream(f);
                        speak.Speak(word);
                    }
                }
                //Create phoneme text file for each word                
                using (FileStream f = new FileStream(word + ".txt", FileMode.Create))
                {
                    //call function to extract the phoneme
                    ExtractPhoneme2 extract = new ExtractPhoneme2();
                    string phoneme = extract.getPhoneme(word);

                    //Encode the phoneme and write  to file
                    Byte[] info = new UTF8Encoding(true).GetBytes(phoneme + "\n");                    
                    f.Write(info, 0, info.Length);
                }
            }
        }
    }
}
