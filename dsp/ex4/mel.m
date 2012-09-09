%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  Tutorial zur Vorlesung "Digitale Signalverarbeitung 2010"
%%%%  URL: www.lsv.uni-saarland.de
%%%%  Datum: Juni 2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [M] = mel(fl,fh, Nfft,Fs,L,fmel);
% fl   = Lower filter frequency
% fh   = Upper filter frequency
% fmel = conversion factor (e.g.: htk: 2595, sphinx: 1125)
% L    = number of filters (e.g. 40)
% Nfft = DFT-length; must be a power of 2. The larger Nfft, the more accurate the filterbank
% Fs   = Sampling Frequency (e.g. 16000);
% Remark: fl and fh are different for HTK to sphinx also...
% OUT: Filterbank M

% Boundaries with respect to filter bank frequencies
Nfft = Nfft/2 + 1;
B_fl = fmel * log(1 + (fl/700));  
B_fh = fmel * log(1 + (fh/700));

f(1) = 0;
for m=2:L+2
  b = B_fl + m * ( (B_fh - B_fl)/L+1);
  f(m) = (Nfft / Fs ) * 700 * (exp(b/1125) - 1);
end;
% you can plot f(m) here if you like to


% Implemented first option of Huang-Acero-Hon book pp.317
H=zeros(L+1, Nfft);   % preallocate memory
for m=2:L+1
  for k=1:Nfft
    if ( k < f(m-1) )
        H(m,k) = 0;
    elseif ( f(m-1) <= k && k <= f(m) )
        H(m,k) = (2 * ( k - f(m-1) )) / ((f(m+1) - f(m-1) ) * ( f(m) - f(m-1) ));
    elseif ( f(m) <= k && k <= f(m+1) )
        H(m,k) = (2 * ( f(m+1) - k )) / ((f(m+1) - f(m-1) ) * (f(m+1) - f(m) ));
    elseif (k > f(m+1))
        H(m,k) = 0;
    else
        fprintf('we got some unexplainable internal error\n');
    end;
  end;
end;

M=zeros(L, Nfft);
M(1:L,:) = H(2:L+1,:);
