using System;

namespace csharp
{
    public class IOException : BionitioException
    {
        public IOException(string filename, Exception inner)
            : base($"Error reading file {filename}", inner)
        {
        }
    }
    
    public class BionitioException : Exception
    {
        public BionitioException(string message) : base(message)
        {
        }

        public BionitioException(string message, Exception inner) : base(message, inner)
        {
        }
    }
}