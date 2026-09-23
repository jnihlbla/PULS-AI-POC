000100 01  FIL-WDR901.                                                          
000200*                                 EKONOMISKA HÄNDELSETRANSAR              
000300*                                 FYSISK NYCKEL: WDR901KY                 
000400*                                 (IDPGM + DAREGDAT + TIKLOCK +           
000500*                                  IDSEKVNR + IDCPYTXT                    
000600*                                 SÖKBEGREPP: IDPGM, DAREGDAT,            
000700*                                 TIKLOCK, IDSEKVNR, IDCPYTXT             
000800     03 FIL-IDPGM            PIC X(8).                                    
000900*                                 PROGRAM IDENTITET                       
001000*                                 PROGRAM INTENTITY                       
001100     03 FIL-DAREGDAT         PIC 9(8).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001300*                                 REGISTRATION DATE (YYYYMMDD)            
001400     03 FIL-TIKLOCK          PIC S9(9)           COMP-3.                  
001500*                                 KLOCKSLAG (TTMMSSTH)                    
001600*                                 TIME OF DAY (HHMMSSTH)                  
001700     03 FIL-IDSEKVNR         PIC S9(3)           COMP-3.                  
001800*                                 GENERELLT SEKVENSNUMMER                 
001900*                                 GENERAL SEQUENCE NUMBER                 
002000     03 FIL-IDCPYTXT.                                                     
002100*                                 COPYTEXT IDENTITET                      
002200*                                 IDENTITY OF A COPYTEXT                  
002300        05 FIL-CT-IDSYSTEM   PIC X(4).                                    
002400*                                 VOLVO VCCS SYSTEMNUMMER                 
002500*                                 VOLVO VCCS SYSTEM NUMBER                
002600        05 FIL-CT-IDPTYP     PIC X(3).                                    
002700*                                 POSTTYP                                 
002800*                                 RECORD TYPE                             
002900        05 FIL-CT-IDVTYP     PIC X.                                       
003000*                                 POSTTYPSVERSION                         
003100*                                 RECORD TYPE VERSION                     
003200     03 FIL-IDUSER           PIC X(8).                                    
003300*                                 ANVÄNDARENS SÄKERHETS ID                
003400*                                 USER SECURITY-IDENTITY                  
003500     03 FIL-WDR901-DATA      PIC X(265).                                  
003600*** END OF VILMAII-COPY LENGTH= 304 BYTES                                 
