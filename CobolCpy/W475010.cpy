000010*** EDIT ALLOWED                                                          
000100 01  W475010-CTX.                                                         
000200*                                 POSTTYP 01 FÖR TULLREST.                
000300*                                                                         
000500     03 TREST01-IDPTYP2      PIC X(2).                                    
000600*                                 POSTTYP                                 
000700     03 TREST01-IDFILAVS     PIC X(8).                                    
000800*                                 FILAVSÄNDARID   = 01441300              
000900     03 TREST01-IDORGNR      PIC X(17).                                   
001000*                                 ORGANISATIONSNR = 5560743089            
001200     03 TREST01-DAFILDAT     PIC 9(8).                                    
001300*                                 FILAVSÄNDNINGSDATUM (CCYYMMDD)          
001500     03 TREST01-TIFILTID     PIC 9(4).                                    
001600*                                 FILAVSÄNDNINGSTID (HHMM)                
001700     03 FILLER               PIC X(41).                                   
001800*                                 FILEN SKALL VARA 80 LÅNG                
002400*** END COPY W475010    LENGTH= 80 BYTES                                  
