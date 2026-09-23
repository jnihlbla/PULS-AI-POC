000100 01  MID-W3I12101.                                                        
000200*                                 MID-COPYTEXT FÖR W3012100               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-KDEXCHA-IN       PIC X(3).                                    
000800*                                 EXCHANGE ACCOUNT CODE                   
000900     03 MID-KDEXCHA-UT       PIC X(3).                                    
001000*                                 EXCHANGE ACCOUNT CODE                   
001100     03 MID-INPT-FLD.                                                     
001200*                                 INPUT FÄLT,UTAN NYCKLAR                 
001300        05 MID-IDDISTR-BET   PIC X(4).                                    
001400*                                 BATALANDE DISTRIKT                      
001500        05 MID-FLEXCRET      PIC X.                                       
001600*                                 EXCHANGE RETURN FLAG                    
001700        05 MID-FLEXCREP      PIC X.                                       
001800*                                 EXCHANGE REPORT FLAG                    
001900        05 MID-FLEXCDET      PIC X.                                       
002000*                                 EXCHANGE REPORT FLAG DET                
002100        05 MID-IDMAIL        PIC X(60).                                   
002200*                                 MAIL ADRESS                             
002300        05 MID-FLFAKT        PIC X.                                       
002400*                                 FAKTURERINGSFLAGGA                      
002500        05 MID-FLRETREM      PIC X.                                       
002600*                                 EXCHANGE REMIND FLAG                    
002700        05 MID-KVVECKOR-BYFA PIC X(3).                                    
002800*                                 ANTAL VECKOR FÖR BYTESFAKTURA           
002900        05 MID-KVVECKOR-BYRE PIC X(3).                                    
003000*                                 ANTAL VECKOR INNAN RENSING              
003100        05 MID-EXCL-FGR      OCCURS 16 TIMES.                             
003200*                                 EJ UPPFÖLJDA BYTESGRUPPER               
003300           07 MID-IDFKNGRP-FOM                                            
003400                             PIC X(4).                                    
003500*                                 FUNKTIONSGRUPP-FROM                     
003600           07 MID-IDFKNGRP-TOM                                            
003700                             PIC X(4).                                    
003800*                                 FUNKTIONSGRUPP-TOM                      
003900*** END OF VILMAII-COPY LENGTH= 217 BYTES                                 
