<span style ="font-size:13px"> Smoothing can enhance signal to noise and uses the SG filter with the polynomial order specified, 3 default usually works well. Derivative transformation uses the order specified. If doing identification with a derivative library, 1 is required, 0 should be used if no derivative transformation is desired. Smoothing uses the SG filter on an window of points, specifying the wavenumber window larger will make the spectra more smooth. The absolute value does something similar to intensity correction to make the spectra more absorbance-like.</span>