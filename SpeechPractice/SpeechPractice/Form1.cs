using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Speech;
using System.Speech.Synthesis;

namespace SpeechPractice
{
    public partial class Form1 : Form
    {
        //create a Speech Synthesizer to speak the text input
        SpeechSynthesizer reader = new SpeechSynthesizer();

        //create the object. This object will store your phonetic 'characters'
       PromptBuilder PBuilder = new PromptBuilder();

        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            if (richTextBox1.Text != "")
            {
                reader.Dispose();
                reader = new SpeechSynthesizer();

                PBuilder = new PromptBuilder();
                //PBuilder.AppendTextWithPronunciation("a", richTextBox1.Text);
                reader.SpeakAsync(richTextBox1.Text);
                ExtractPhoneme extract = new ExtractPhoneme();
                extract.getPhoneme(richTextBox1.Text);
            }
            else
            {
                MessageBox.Show("Please enter some text first.");
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            if (reader != null)
            {
                if (reader.State == SynthesizerState.Speaking)
                {
                    reader.Pause();
                }
            }
        }

        private void button3_Click(object sender, EventArgs e)
        {
            if (reader != null)
            {
                if (reader.State == SynthesizerState.Paused)
                {
                    reader.Resume();
                }
            }
        }

        private void button4_Click(object sender, EventArgs e)
        {
            if (reader != null)
            {
                reader.Dispose();
            }
        }
    }
}
