000010 01  W22149.                                                              
000020*                                 COPYTEXT FÖR FILEN W22149               
000030*                                 PB-TPO OCH PB-VESL                      
000040*                                                                         
000050     03 IDARTNR              PIC S9(9)           COMP-3.                  
000060*                                 ARTIKELNUMMER                           
000070     03 KDGK                 PIC S9              COMP-3.                  
000080*                                 GODSMOTTAGAREKOD                        
000090     03 W22149-001           OCCURS 2 TIMES.                              
000100*                                                                         
000110*                                                                         
000120        05 KVPB-VESL-GAMMAL  PIC S9(6)V9(1)      COMP-3.                  
000130*                                 GÄLLANDE PB VID VECKOSLUT               
000140        05 KVPB-VESL-NY      PIC S9(6)V9(1)      COMP-3.                  
000150*                                 GÄLLANDE PB VID VECKOSLUT               
000160        05 KVPB-TPO-GAMMAL   PIC S9(6)V9(1)      COMP-3.                  
000170*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
000180        05 KVPB-TPO-NY       PIC S9(6)V9(1)      COMP-3.                  
000190*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
      *** END COPY W22149      LENGTH=38                                        
