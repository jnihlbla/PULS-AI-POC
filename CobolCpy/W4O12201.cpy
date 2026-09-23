000100 01  MOD-W4O12201.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O122                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDKVAOMR-IN      PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200*                                 MFS DISPOSITION OF INPUT FIELD          
001300     03 MOD-IDKVAGRP-IN      PIC X(3).                                    
001400*                                 KVALITET KONTROLLGRUPP                  
001500*                                 QUALITY CONTROL GROUP                   
001600     03 MOD-TIAARP-IN        PIC X(4).                                    
001700*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
001800*                                 12 PER ÅR                               
001900*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002000*                                 12 PER YEAR                             
002100     03 MOD-KDKVASTA-IN      PIC X(2).                                    
002200*                                 MFS BEHANDLING AV INPUTFÄLT             
002300*                                 MFS DISPOSITION OF INPUT FIELD          
002400     03 MOD-IDDC-IN          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700     03 MOD-IDKVAOMR-UT      PIC X.                                       
002800*                                 KVALITET KONTROLLOMRÅDE                 
002900*                                 QUALITY CONTROL AREA                    
003000     03 MOD-IDKVAGRP-UT      PIC X(3).                                    
003100*                                 KVALITET KONTROLLGRUPP                  
003200*                                 QUALITY CONTROL GROUP                   
003300     03 MOD-TIAARP-UT        PIC 9(4) BLANK WHEN ZERO.                    
003400*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
003500*                                 12 PER ÅR                               
003600*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
003700*                                 12 PER YEAR                             
003800     03 MOD-KDKVASTA-UT      PIC X(2).                                    
003900*                                 MFS BEHANDLING AV INPUTFÄLT             
004000*                                 MFS DISPOSITION OF INPUT FIELD          
004100     03 MOD-IDDC-UT          PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300*                                 WAREHOUSE IDENTIFIER                    
004400     03 MOD-IDKVAOMR-EN      PIC X.                                       
004500*                                 KVALITET KONTROLLOMRÅDE                 
004600*                                 QUALITY CONTROL AREA                    
004700     03 MOD-IDKVAGRP-EN      PIC X(3).                                    
004800*                                 KVALITET KONTROLLGRUPP                  
004900*                                 QUALITY CONTROL GROUP                   
005000     03 MOD-TIREGDAT-EN      PIC 9(6).                                    
005100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005200*                                 REGISTRATION DATE (YYMMDD)              
005300     03 MOD-KDKVASTA-EN      PIC X.                                       
005400*                                 KVALITET LAGERREVISION STATUS           
005500*                                 QUALITY WAREHOUSE AUDIT STATUS          
005600     03 MOD-IDKVAOMR-NX      PIC X.                                       
005700*                                 KVALITET KONTROLLOMRÅDE                 
005800*                                 QUALITY CONTROL AREA                    
005900     03 MOD-IDKVAGRP-NX      PIC X(3).                                    
006000*                                 KVALITET KONTROLLGRUPP                  
006100*                                 QUALITY CONTROL GROUP                   
006200     03 MOD-TIREGDAT-NX      PIC 9(6).                                    
006300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006400*                                 REGISTRATION DATE (YYMMDD)              
006500     03 MOD-KDKVASTA-NX      PIC X.                                       
006600*                                 KVALITET LAGERREVISION STATUS           
006700*                                 QUALITY WAREHOUSE AUDIT STATUS          
006800     03 MOD-RAD              OCCURS 13 TIMES                              
006900                             INDEXED MOD-IX-1.                            
007000*                                  TABELL-RADER                           
007100*                                                                         
007200        05 MOD-VALKOD-ATTR   PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-VALKOD        PIC X.                                       
007500*                                 ÄNDRINGSFLAGGA                          
007600*                                 MARK OF CHANGES                         
007700        05 MOD-IDKVAOMR      PIC X.                                       
007800*                                 KVALITET KONTROLLOMRÅDE                 
007900*                                 QUALITY CONTROL AREA                    
008000        05 MOD-IDKVAGRP      PIC Z(2)9.                                   
008100*                                 KVALITET KONTROLLGRUPP                  
008200*                                 QUALITY CONTROL GROUP                   
008300        05 MOD-TIREGDAT      PIC 9(6).                                    
008400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008500*                                 REGISTRATION DATE (YYMMDD)              
008600        05 MOD-TIKVAKON      PIC 9(6).                                    
008700*                                 KVALITET DATUM FÖR STATISTIK            
008800*                                 QUALITY DATE FOR STATISTIC              
008900        05 MOD-KVART         PIC Z(6)9.                                   
009000*                                 ANTAL ARTNR PER BRYTBEGREPP             
009100*                                 NO OF PARTNOS PER TYPE                  
009200        05 MOD-KDKVASTA      PIC X.                                       
009300*                                 KVALITET LAGERREVISION STATUS           
009400*                                 QUALITY WAREHOUSE AUDIT STATUS          
009500        05 MOD-TIUPPDAT      PIC 9(6).                                    
009600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
009700*                                 UPDATING DATE     (YYMMDD)              
009800     03 MOD-TEMFSINF         PIC X(55).                                   
009900*                                 INFORMATIONSMEDDELANDE                  
010000*                                 INFORMATION MESSAGE                     
010100*** END OF VILMAII-COPY LENGTH= 575 BYTES                                 
