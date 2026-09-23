000100 01  FIL-WDR601.                                                          
000200*                                 TRANSAR TILL BATCH                      
000300*                                 DISTRIBUTION                            
000400*                                 FR≈N ONLINESYSTEMET                     
000500*                                 FYSISK NYCKEL: WDR601KY                 
000600*                                 (IDPGM + TIREGDAT + TIKLOCK +           
000700*                                  IDSEKVNR + IDCPYTXT)                   
000800*                                 S÷KBEGREPP: IDPGM, TIREGDAT,            
000900*                                 TIKLOCK, IDSEKVNR, IDCPYTXT             
001000     03 FIL-IDPGM            PIC X(8).                                    
001100*                                 PROGRAM IDENTITET                       
001200*                                 PROGRAM INTENTITY                       
001300     03 FIL-TIREGDAT         PIC S9(7)           COMP-3.                  
001400*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001500*                                 REGISTRATION DATE (YYMMDD)              
001600     03 FIL-TIKLOCK          PIC S9(9)           COMP-3.                  
001700*                                 KLOCKSLAG (TTMMSSTH)                    
001800*                                 TIME OF DAY (HHMMSSTH)                  
001900     03 FIL-IDSEKVNR         PIC S9(3)           COMP-3.                  
002000*                                 GENERELLT SEKVENSNUMMER                 
002100*                                 GENERAL SEQUENCE NUMBER                 
002200     03 FIL-IDCPYTXT.                                                     
002300*                                 COPYTEXT IDENTITET                      
002400*                                 IDENTITY OF A COPYTEXT                  
002500        05 FIL-CT-IDSYSTEM   PIC X(4).                                    
002600*                                 VOLVO VCCS SYSTEMNUMMER                 
002700*                                 VOLVO VCCS SYSTEM NUMBER                
002800        05 FIL-CT-IDPTYP     PIC X(3).                                    
002900*                                 POSTTYP                                 
003000*                                 RECORD TYPE                             
003100        05 FIL-CT-IDVTYP     PIC X.                                       
003200*                                 POSTTYPSVERSION                         
003300*                                 RECORD TYPE VERSION                     
003400     03 FIL-WDR601-DATA      PIC X(300).                                  
003500*** END OF VILMAII-COPY LENGTH= 327 BYTES                                 
