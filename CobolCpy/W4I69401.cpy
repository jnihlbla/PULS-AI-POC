000100 01  W4I69401-CTX.                                                        
000200*                                 MID-COPYTEXT (FRÅN RELEASE-PGM)         
000300*                                 FÖR W40694                              
000400     03 IDDISTR-UT           PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR-UT          PIC X(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 KDFRAKT-UT           PIC X(2).                                    
000900*                                 FRAKTSÄTT DC TILL KUND                  
001000     03 IDDC-UT              PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 FLLANDROVER          PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400     03 IDORDNR-KOMPL        PIC 9(5).                                    
001500*                                 ORDERNUMMER UTGÅR PD90                  
001600     03 IDPRODNR-KOMPL       PIC 9(7).                                    
001700*                                 PRODUKTIONSNUMMER                       
001800     03 W4I69401-001-GRP     OCCURS 13 TIMES.                             
001900        05 RAD-KDUPPTYP      PIC X.                                       
002000*                                 UPPDATERINGSTYP                         
002100        05 RAD-IDDISTR       PIC 9(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300        05 RAD-IDKUNDNR      PIC 9(6).                                    
002400*                                 KUNDNUMMER                              
002500        05 RAD-KDFRAKT       PIC 9(2).                                    
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700        05 RAD-IDORDNR       PIC 9(5).                                    
002800*                                 ORDERNUMMER UTGÅR PD90                  
002900        05 RAD-KDORDKL       PIC 9.                                       
003000*                                 ORDERKLASS                              
003100        05 RAD-IDPRODNR      PIC 9(7).                                    
003200*                                 PRODUKTIONSNUMMER                       
003300*** END OF VILMAII-COPY LENGTH= 365 BYTES                                 
