package java.lang;
import apple2.*;

public final class Float extends Number
{
	private float value;
	public final static float MAX_VALUE = 0x7FFFFFFF;
	public final static float MIN_VALUE = 0x80000000;
	public final static float NaN = 0x80000000;
	public final static float POSITIVE_INFINITY = 0x80000000;
	public final static float NEGATIVE_INFINITY = 0x80000000;
	private final static float[] twoPow10 =	{1E+1F,1E+2F,1E+4F,1E+8F,1E+16F,1E+32F,0F,0F};
	private final static float[] pow10 =	{1E+0F,1E+1F,1E+2F,1E+3F,1E+4F,1E+5F,1E+6F,1E+7F,1E+8F,
				1E+9F,1E+10F,1E+11F,1E+12F,1E+13F,1E+14F,1E+15F,1E+16F,1E+17F,
				1E+18F,1E+19F,1E+20F,1E+21F,1E+22F,1E+23F,1E+24F,1E+25F,1E+26F,
				1E+27F,1E+28F,1E+29F,1E+30F,1E+31F,1E+32F,1E+33F,1E+34F,1E+35F,1E+36F,
				1E+37F,1E+38F};

	public Float(float fp)
	{
		value = fp;
	}
	public Float(String s)
	{
		value = parseFloat(s);
	}
	
	
	public float floatValue(){return value;}
	public int intValue(){return (int)value;}
	public static int floatToIntBits(float fp){return vm02.floatAsBits(fp);}
	public static float intToFloatBits(int i){return vm02.bitsAsFloat(i);}
	public static float trunc(float fp)
	{
		int mask, exp, fpBits = vm02.floatAsBits(fp);
		
		exp  = (fpBits >> 23) & 0xFF;
		if (exp >= 151) // fp too big for any valid frac bits
			return fp;
		if (exp < 127) // fp already smaller than 1.0
			return 0.0F;
		//
		// Mask off fractional bits
		//
		mask = 0x007FFFFF >> (exp - 127);
		return vm02.bitsAsFloat(fpBits & ~mask);
	}
	public static float frac(float fp)
	{
		int sign, exp, mant, fpBits = vm02.floatAsBits(fp);
		
		exp  = (fpBits >> 23) & 0xFF;
		if (exp >= 151) // fp too big for any valid frac bits
			return 0.0F;
		if (exp < 127) // fp already smaller than 1.0
			return fp;
		sign =  fpBits & 0x80000000;
		mant = (fpBits & 0x00FFFFFF) | 0x00800000;
		//
		// Mask off bits above 1.0
		//
		mant &= 0x007FFFFF >> (exp - 127);
		if (mant == 0)
			return 0.0F;
		//
		// Normalize mantissa
		//
		while ((mant & 0x00800000) == 0)
		{
			exp--;
			mant <<= 1;
		}
		return vm02.bitsAsFloat(sign | (exp << 23) | (mant & 0x007FFFFF));
	}
	public static Float valueOf(String s){return new Float(s);}
	public static float parseFloat(String s)
	{
		float   fp = 0.0F;
		float   scale;
		char    c;
		int     place, digit;
		int     scanPos = 0;
		int     dp      = 0;
		boolean neg     = false;
		boolean negExp  = false;

		//
		// Check sign
		//
		c = s.charAt(0);
		if (c == '-')
		{
			neg = true;
			scanPos++;
		}
		else if (c == '+')
			scanPos++;
		//
		// Scan integer
		//
		while (scanPos < s.length())
		{
			c = s.charAt(scanPos++);
			digit = c - '0';
			if (digit < 0 || digit > 9)
				break;
			fp = (fp * 10.0F) + digit;
		}
		if (c == '.')
		{
			//
			// Scan fractional
			//
			scanPos++;
			while (scanPos < s.length())
			{
				c = s.charAt(scanPos++);
				digit = c - '0';
				if (digit < 0 || digit > 9)
					break;
				dp--;
				fp = (fp * 10.0F) + digit;
			}
		}
		if ((c == 'e' || c == 'E') && (scanPos < s.length() - 1))
		{
			//
			// Scan exponent
			//
			int exp = 0;
			
			scanPos++;
			if (s.charAt(scanPos) == '-')
			{
				negExp = true;
				scanPos++;
			}
			else if (s.charAt(scanPos) == '+')
				scanPos++;
			while (scanPos < s.length())
			{
				c = s.charAt(scanPos++);
				digit = c - '0';
				if (digit < 0 || digit > 9)
					break;
				exp = exp * 10 + digit;
			}
			if (negExp)
				exp = -exp;
			dp += exp;
		}
		if (dp < 0)
		{
			dp = -dp;
			negExp = true;
		}
		else
			negExp = false;
		//
		// Apply scale
		//
		for (place = 0, scale = 1.0F; dp != 0; dp >>= 1, place++)
			if ((dp & 1) != 0)
				scale *= twoPow10[place];
		if (negExp)
			fp /= scale;
		else
			fp *= scale;
		return neg ? -fp : fp;
	}
	public String toString(){return toString(value);}
	public static String toString(float fp)
	{
		byte buffer[] = new byte[33];
		boolean skipZero, neg = false;
		int buffptr, refStr, scanPos = 32;
		int  dp, fp10, exp10, digit, exp, mant, fpBits = vm02.floatAsBits(fp);
		float fpsci = 0.0F;
		
		if (fpBits < 0)
		{
			fp = -fp;
			neg = true;
		}
		exp    = ((fpBits >> 23) & 0xFF) - 127;
		mant   = (fpBits & 0x00FFFFFF) | 0x00800000;
		//
		// Convert float to number between 1.0 and 10.0
		// with exp10 being orignal power of ten.
		//
		if (fp == 0.0F)
		{
			exp10 = 0;
			fp10  = 0;
		}
		else
		{
			//
			// Law of logarithms  LOG10 X = LOG10 2 * LOG2 X
			//                            = .30103 * LOG2 X
			//                            ~ 154 / 512 * LOG2 X
			//
			exp10 = (exp * 154) >> 9;
			fpsci = (exp10 < 0) ? (fp * pow10[-exp10]) : (fp / pow10[exp10]);
			//
			// Make adjustment if needed
			//
			if (fpsci >= 10.0F)
			{
				fpsci *= 0.1F;
				exp10++;
			}
			else if (fpsci < 1.0F)
			{
				fpsci *= 10.0F;
				exp10--;
			}
		}
		if (exp10 < 6 && exp10 > -4)
		{
			//
			// Standard floating point - 4 fractional digits
			//
			dp     = ((exp10 < 0) ? -exp10 : 0) + 4;
			fp10  = (int)(fp * pow10[dp] + 0.5F);
		}
		else
		{
			//
			// Scientific notation - 5 fractional digits
			//
			dp     = 5;
			fp10  = (int)(fpsci * 100000.0F + 0.5F);
			//
			// Print exponent
			//
			buffer[scanPos--] = (byte)(exp10 % 10 | '0');
			buffer[scanPos--] = (byte)(exp10 / 10 | '0');
			buffer[scanPos--] = (exp10 < 0) ? (byte)'-' : (byte)'+';
			buffer[scanPos--] = (byte)'E';
		}
		//
		// Print fractional number
		//
		skipZero = true;
		while (--dp > 0)
		{
			digit = fp10 % 10;
			fp10 /= 10;
			if (!skipZero || digit != 0)
			{
				buffer[scanPos--] = (byte)('0' + digit);
				skipZero = false;
			}
		}
		buffer[scanPos--] = (byte)('0' + fp10 % 10);
		buffer[scanPos--] = (byte)'.';
		fp10 /= 10;
		//
		// Print integral number
		//
		do
		{
			buffer[scanPos--] = (byte)('0' + (fp10 % 10));
			fp10 /= 10;
		} while (fp10 > 0);
		if (neg)
			buffer[scanPos--] = (byte)'-';
		buffer[scanPos] = (byte)(32 - scanPos);
		buffptr = vm02.call(vm02.refAsBits((Object)buffer), 0x0E) + 2 + scanPos ; // HMEM_LOCK
		refStr = vm02.call(buffptr, 0x46); // HSTRPL_ADD
		vm02.call(vm02.refAsBits((Object)buffer), 0x10); // HMEM_UNLOCK
		return (String)vm02.bitsAsRef((refStr & 0xFFFF) | 0x00830000 | ((refStr << 8) & 0xFF000000));
	}
}