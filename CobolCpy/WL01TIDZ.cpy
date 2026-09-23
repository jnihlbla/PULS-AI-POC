000100 01  MSGI-WL01TIDZ.                                                       
000200*                                 BESKRIVNING AV GENERELL                 
000300*                                 RUTIN FÖR OMVANDLING AV                 
000400*                                 TID/DATUM                               
000500     03 MSGI-KDCALL          PIC X(3).                                    
000600*                                 ANROPSTYP                               
000700*                                 CALL TYPE                               
000800     03 MSGI-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 MSGI-IDTIDZON        PIC X(2).                                    
001200*                                 TIDZONER PÅ JORDEN.                     
001300*                                 TIME ZONE ON EARTH                      
001400     03 MSGI-TILOKDAT        PIC X(6).                                    
001500*                                 DATUM FÖR LOKAL TID     AAMMDD          
001600*                                 DATE FOR LOCAL TIME     YYMMDD          
001700     03 MSGI-TILOKTID        PIC X(4).                                    
001800*                                 TID (KLOCKAN) FÖR LOKAL TID             
001900*                                 LOCAL TIME AS  HHMM                     
002000     03 MSGI-KDSVAR          PIC X.                                       
002100*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002200*                                 RETURN CODE FROM PROGRAM                
002300*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
