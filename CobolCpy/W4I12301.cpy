000100 01  MID-W4I12301.                                                        
000200*                                 COPYTEXT FÖR MID W4I12301               
000300*                                                                         
000400     03 MID-IDKVAOMRI        PIC X.                                       
000500*                                 KVALITET KONTROLLOMRÅDE                 
000600*                                 QUALITY CONTROL AREA                    
000700     03 MID-IDKVAGRPI        PIC X(3).                                    
000800*                                 KVALITET KONTROLLGRUPP                  
000900*                                 QUALITY CONTROL GROUP                   
001000     03 MID-TIAARPI          PIC X(4).                                    
001100*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
001200*                                 12 PER ÅR                               
001300*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
001400*                                 12 PER YEAR                             
001500     03 MID-IDDCIN           PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 MID-IDKVAOMRU        PIC X.                                       
001900*                                 KVALITET KONTROLLOMRÅDE                 
002000*                                 QUALITY CONTROL AREA                    
002100     03 MID-IDKVAGRPU        PIC X(3).                                    
002200*                                 KVALITET KONTROLLGRUPP                  
002300*                                 QUALITY CONTROL GROUP                   
002400     03 MID-TIAARPU          PIC X(4).                                    
002500*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
002600*                                 12 PER ÅR                               
002700*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002800*                                 12 PER YEAR                             
002900     03 MID-IDDCUT           PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100*                                 WAREHOUSE IDENTIFIER                    
003200     03 MID-IDARTNR-EN       PIC 9(8).                                    
003300*                                 ARTIKELNUMMER                           
003400*                                 PART NUMBER                             
003500     03 MID-IDKVAFEL-EN      PIC X(2).                                    
003600*                                 KVALITET FELKOD FÖR ARTIKEL             
003700*                                 ERROR CODE FOR PARTNUMBER               
003800     03 MID-IDARTNR-NX       PIC 9(8).                                    
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100     03 MID-IDKVAFEL-NX      PIC X(2).                                    
004200*                                 KVALITET FELKOD FÖR ARTIKEL             
004300*                                 ERROR CODE FOR PARTNUMBER               
004400     03 MID-INPUT.                                                        
004500*                                 COPYTEXT FOR MID W4O11501 ENDAS         
004600*                                 T INDATA-FÄLT                           
004700        05 MID-RAD           OCCURS 14 TIMES.                             
004800*                                 MID-COPYTEXT FÖR R4F123                 
004900           07 MID-SPALT      OCCURS 4 TIMES.                              
005000*                                 MID-COPYTEXT FÖR R4F123                 
005100              09 MID-IDARTNR-IN                                           
005200                             PIC 9(8).                                    
005300*                                 ARTIKELNUMMER                           
005400*                                 PART NUMBER                             
005500              09 MID-IDKVAFEL-IN                                          
005600                             PIC 9(2).                                    
005700*                                 KVALITET FELKOD FÖR ARTIKEL             
005800*                                 ERROR CODE FOR PARTNUMBER               
005900              09 MID-KDCMD-IN                                             
006000                             PIC X.                                       
006100               88 MID-KDCMD-INGENTING                                     
006200                             VALUE ' '.                                   
006300               88 MID-KDCMD-DELETE                                        
006400                             VALUE 'D'                                    
006500                             'B'.                                         
006600               88 MID-KDCMD-REPLACE                                       
006700                             VALUE 'R'                                    
006800                             'Ä'.                                         
006900               88 MID-KDCMD-INSERT                                        
007000                             VALUE 'I'                                    
007100                             'N'.                                         
007200*                                 RAD-UPPDATERINGSKOMMANDO                
007300*                                 LINE UPDATE COMMAND                     
007400*** END OF VILMAII-COPY LENGTH= 656 BYTES                                 
