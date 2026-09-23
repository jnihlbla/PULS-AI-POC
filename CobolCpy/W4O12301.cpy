000100 01  MOD-W4O12301.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR 4123                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDKVAOMRI        PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200*                                 MFS DISPOSITION OF INPUT FIELD          
001300     03 MOD-IDKVAGRPI        PIC X(2).                                    
001400*                                 MFS BEHANDLING AV INPUTFÄLT             
001500*                                 MFS DISPOSITION OF INPUT FIELD          
001600     03 MOD-TIAARPI          PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800*                                 MFS DISPOSITION OF INPUT FIELD          
001900     03 MOD-IDDCIN           PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100*                                 MFS DISPOSITION OF INPUT FIELD          
002200     03 MOD-IDKVAOMRU        PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400*                                 MFS DISPOSITION OF INPUT FIELD          
002500     03 MOD-IDKVAGRPU        PIC X(3).                                    
002600*                                 KVALITET KONTROLLGRUPP                  
002700*                                 QUALITY CONTROL GROUP                   
002800     03 MOD-TIAARPU          PIC 9(4) BLANK WHEN ZERO.                    
002900*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
003000*                                 12 PER ÅR                               
003100*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
003200*                                 12 PER YEAR                             
003300     03 MOD-IDDCUT           PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500*                                 WAREHOUSE IDENTIFIER                    
003600     03 MOD-IDARTNR-EN       PIC Z(7)9.                                   
003700*                                 ARTIKELNUMMER                           
003800*                                 PART NUMBER                             
003900     03 MOD-IDKVAFEL-EN      PIC Z9.                                      
004000*                                 KVALITET FELKOD FÖR ARTIKEL             
004100*                                 ERROR CODE FOR PARTNUMBER               
004200     03 MOD-IDARTNR-NX       PIC Z(7)9.                                   
004300*                                 ARTIKELNUMMER                           
004400*                                 PART NUMBER                             
004500     03 MOD-IDKVAFEL-NX      PIC Z9.                                      
004600*                                 KVALITET FELKOD FÖR ARTIKEL             
004700*                                 ERROR CODE FOR PARTNUMBER               
004800     03 MOD-RAD              OCCURS 14 TIMES.                             
004900        05 MOD-SPALT         OCCURS 4 TIMES.                              
005000           07 MOD-IDARTNR-UT-ATTR                                         
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300           07 MOD-IDARTNR-UT PIC Z(7)9.                                   
005400*                                 ARTIKELNUMMER                           
005500*                                 PART NUMBER                             
005600           07 MOD-IDKVAFEL-UT-ATTR                                        
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900           07 MOD-IDKVAFEL-UT                                             
006000                             PIC Z9.                                      
006100*                                 KVALITET FELKOD FÖR ARTIKEL             
006200*                                 ERROR CODE FOR PARTNUMBER               
006300           07 MOD-KDCMD-UT-ATTR                                           
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600           07 MOD-KDCMD-UT   PIC X(2).                                    
006700*                                 MFS BEHANDLING AV INPUTFÄLT             
006800*                                 MFS DISPOSITION OF INPUT FIELD          
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*                                 INFORMATION MESSAGE                     
007200*** END OF VILMAII-COPY LENGTH= 1146 BYTES                                
