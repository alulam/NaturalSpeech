using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Net;
using System.IO;
using System.Web;


namespace SoundGeneration
{
    public class ExtractPhoneme
    {
        public string getPhoneme(string input)
        {      
            // Create a request using a URL that can receive a post. 
            HttpWebRequest request = (HttpWebRequest)WebRequest.Create("http://wikspeak.sourceforge.net/cgi-bin/ipa.cgi");
            
            // Set the Method property of the request to POST.
            request.Method = "POST";
            request.KeepAlive = true;
            
            //Create boundary for multipart form
            string boundary = CreateFormDataBoundary();
        
            // Set the ContentType property of the WebRequest.
            request.ContentType = "multipart/form-data; boundary=" + boundary;
            
            // Get the request stream.
            Stream dataStream = request.GetRequestStream();
            
            // Write the data to the request stream.
            writeMultiForm(input, dataStream, boundary);

            //Create Footer for multipart form
            byte[] endBytes = Encoding.UTF8.GetBytes("--" + boundary + "--");
            dataStream.Write(endBytes, 0, endBytes.Length);
            
            // Close the Stream object.
            dataStream.Close();

            // Get the response.
            try {
                WebResponse response = request.GetResponse();
                // Display the status.
                Console.WriteLine(((HttpWebResponse)response).StatusDescription);
                
                // Get the stream containing content returned by the server.
                dataStream = response.GetResponseStream();
                
                // Open the stream using a StreamReader for easy access.
                StreamReader reader = new StreamReader(dataStream);
                
                // Read the content.
                string responseFromServer = reader.ReadToEnd();
                
                //parse string for phoneme
                int startOffset = 18;
                int startLocation = responseFromServer.IndexOf("name=\"ipa\"") + startOffset;

                int endOffset = 2;
                int endLocation = responseFromServer.IndexOf("readonly") - endOffset;
                int length = endLocation - startLocation;
                
                string phoneme = responseFromServer.Substring(startLocation, length);
                //var encoding = System.Text.GetEncoding(phoneme);
                Console.WriteLine(phoneme);

                //Attempt to convert phoneme to english through code
                string s = WebUtility.HtmlDecode(phoneme);
                Console.WriteLine(s);

                //Remove apostrophes
                phoneme = phoneme.Replace("&#x02c8;", "");

                //Remove colons
                phoneme = phoneme.Replace(":", "");

                // Clean up the streams.
                reader.Close();
                dataStream.Close();
                response.Close();

                return phoneme;
            }
            catch(WebException e)
            {
                Console.WriteLine("This program is expected to throw WebException on successful run."+
                       "\n\nException Message :" + e.Message);
            }
            return null;
        }
        public static string CreateFormDataBoundary()
        {
            return "---------------------------" + DateTime.Now.Ticks.ToString("x");
        }

        public const string FormDataTemplate = "--{0}\r\nContent-Disposition: form-data; name=\"{1}\"\r\n\r\n{2}\r\n";

        public static void writeMultiForm(string input, Stream stream, string boundary)
        {
            if (stream == null)
            {
                throw new ArgumentNullException("stream");
            }
            string item = String.Format(FormDataTemplate, boundary, "word", input);
            byte[] itemBytes = Encoding.UTF8.GetBytes(item);
            stream.Write(itemBytes, 0, itemBytes.Length);
        }

    }
}
