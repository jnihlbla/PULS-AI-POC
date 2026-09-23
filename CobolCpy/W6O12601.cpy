000100 01  MOD-W6O12601-CTX.                                                    
000200*                                 MOD FÖR PROGRAM W60126                  
000300*                                 PROGRAMMET VISAR PLACERINGS-            
000400*                                 HISTORIK FÖR INLEVERANS                 
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-IDLOPNRM-IN-ATTR PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
001200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001300*                                 (0VVDLLLLK)                             
001400     03 MOD-IDLOPNRM-UT-ATTR PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900     03 MOD-IDRADNR-IN-ATTR  PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDRADNR-IN       PIC X(4).                                    
002200*                                 RADNUMMER                               
002300     03 MOD-IDRADNR-UT-ATTR  PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-IDRADNR-UT       PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-W6O12601-001-GRP OCCURS 14 TIMES.                             
002800*                                 RADER SOM VISAR PLACERINGSHIS-          
002900*                                 TORIK                                   
003000        05 MOD-IDRADNR-ATTR  PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-IDRADNR       PIC X(4).                                    
003300*                                 RADNUMMER                               
003400        05 MOD-TIREGDAT-ATTR PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-TIREGDAT      PIC X(6).                                    
003700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003800        05 MOD-TIHHMM-ATTR   PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-TIHHMM        PIC X(5).                                    
004100*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004200        05 MOD-KDINLSTA-ATTR PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KDINLSTA      PIC X(3).                                    
004500*                                 SYSTEMSTATUS INLEVERANS                 
004600        05 MOD-ADINLOMR-ATTR PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-ADINLOMR      PIC X(4).                                    
004900*                                 INLEVERANSOMRÅDE                        
005000        05 MOD-ADINLOMR-NXT-ATTR                                          
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-ADINLOMR-NXT  PIC X(4).                                    
005400*                                 INLEVERANSOMRÅDE                        
005500        05 MOD-KVINLART-ATTR PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-KVINLART      PIC X(6).                                    
005800*                                 ANTAL I PARTIRAD                        
005900        05 MOD-IDUSER-ATTR   PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-IDUSER        PIC X(8).                                    
006200*                                 ANVÄNDARENS SÄKERHETS ID                
006300     03 MOD-TEMFSINF         PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 915 BYTES                                 
