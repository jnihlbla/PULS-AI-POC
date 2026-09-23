000100 01  W425W004.                                                            
000200*                                 LÄNKAREA  4 FÖR W42510                  
000300*                                                                         
000400     03 ADGODSMK             PIC X(54).                                   
000500*                                 GODSMOTTAGARADRESS                      
000600     03 BEGODSM              PIC X(54).                                   
000700*                                 GODSMOTTAGARNAMN                        
000800     03 BEKUNDRF             PIC X(10).                                   
000900*                                 KUNDENS REFERENS                        
001000     03 KDFAKTYP             PIC X.                                       
001100*                                 FAKTURATYP                              
001200     03 KDPRGRP              PIC S9(3)           COMP-3.                  
001300*                                 PRISGRUPP                               
001400     03 KDSPRAK              PIC S9              COMP-3.                  
001500*                                 SPRÅKKOD                                
001600     03 KDTULLVE             PIC S9              COMP-3.                  
001700*                                 TYP AV PRIS PÅ TULLFAKTURA              
001800     03 REOMRTAL             PIC S9(2)V9(3)      COMP-3.                  
001900*                                 OMRÄKNINGSTAL                           
002000     03 TIPLLEVD             PIC S9(3)           COMP-3.                  
002100*                                 PLANERAD LEVERANSDAG (VVD)              
002200     03 TIREF1               PIC S9(7)           COMP-3.                  
002300*                                 KUNDREFERENS-DATUM 1   (ÅÅMMDD)         
002400*** END COPY W425W004C0  LENGTH=132                                       
