010000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
020000* AVSER   :  BESTÄLLNINGSPOST                                   *         
030000* URSPRUNG:                                                     *         
040000* INNEHÅLL:                                                     *         
050000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
060000 01  A311T940.                                                            
070000     03  PT                      PIC S9(3)         COMP-3.                
080000     03  ARTNR                   PIC S9(9)         COMP-3.                
090000     03  GSDB-LEV                PIC X(5).                                
100000     03  LEVSUFF                 PIC X(1).                                
110000     03  GSDB-FORB               PIC X(5).                                
120000     03  BESTNR.                                                          
130000         05  BESTPREF            PIC S9(3)         COMP-3.                
140000         05  BESTLNR             PIC S9(7)         COMP-3.                
150000         05  BESTSUFF            PIC S9(3)         COMP-3.                
160000     03  DATUM-UTSKR             PIC S9(7)         COMP-3.                
170000     03  ANT-BESTANN             PIC S9(9)         COMP-3.                
180000     03  BESTPRIS-UPPG           PIC X(8).                                
190000     03  FILLER REDEFINES BESTPRIS-UPPG.                                  
200000         05  BESTPRIS            PIC S9(9)         COMP-3.                
210000         05  KDVAL-BEST          PIC S9(3)         COMP-3.                
220000         05  KDENH-BEST          PIC X(1).                                
230000     03  KDSORT                  PIC X(1).                                
240000     03  KDVAL-ISO               PIC X(3).                                
250000     03  LEVNUM                  PIC S9(5)         COMP-3.                
260000     03  FORBNR                  PIC S9(5)         COMP-3.                
260100     03  IDLEVNR-SHIP            PIC X(05).                               
270000*** END OF VILMAII-COPY LENGTH= 58 BYTES                                  
