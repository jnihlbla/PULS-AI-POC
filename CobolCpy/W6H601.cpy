000100 01  OMR-W6H601.                                                          
000200*                                 KVALITET                                
000300*                                 LAGERKONTROLL                           
000400*                                 FYSISK NYCKEL: W6H601KY                 
000500*                                 (IDDC     IDKVAOMR                      
000600*                                  IDKVAGRP DAREGDAT)                     
000700     03 OMR-IDDC             PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 OMR-IDKVAOMR         PIC X.                                       
001100*                                 KVALITET KONTROLLOMR≈DE                 
001200*                                 QUALITY CONTROL AREA                    
001300     03 OMR-IDKVAGRP         PIC 9(3).                                    
001400*                                 KVALITET KONTROLLGRUPP                  
001500*                                 QUALITY CONTROL GROUP                   
001600     03 OMR-DAREGDAT         PIC 9(8).                                    
001700*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001800*                                 REGISTRATION DATE (YYYYMMDD)            
001900     03 OMR-TIKVAKON         PIC S9(7)           COMP-3.                  
002000*                                 KVALITET DATUM F÷R STATISTIK            
002100*                                 QUALITY DATE FOR STATISTIC              
002200     03 OMR-KVART            PIC S9(7)           COMP-3.                  
002300*                                 ANTAL ARTNR PER BRYTBEGREPP             
002400*                                 NO OF PARTNOS PER TYPE                  
002500     03 OMR-KDKVASTA         PIC X.                                       
002600*                                 KVALITET LAGERREVISION STATUS           
002700*                                 QUALITY WAREHOUSE AUDIT STATUS          
002800     03 OMR-TIUPPDAT         PIC S9(7)           COMP-3.                  
002900*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
003000*                                 UPDATING DATE     (YYMMDD)              
003100*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
