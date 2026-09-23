000100 01  MOD-W4O21401.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O21401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR-IN       PIC X(5).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MOD-IDORDNR-UT       PIC X(5).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-IDDC-UT          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-TEDDI            PIC X(11).                                   
002200*                                 TEXTFÄLT DDI                            
002300     03 MOD-BEBET.                                                        
002400*                                 BETALNINGSANSVARIG NAMN                 
002500        05 MOD-BEBETRAD-1    PIC X(35).                                   
002600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002700        05 MOD-BEBETRAD-2    PIC X(35).                                   
002800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002900     03 MOD-ADBET.                                                        
003000*                                 BETALNINGSANSVARIG ADRESS               
003100        05 MOD-ADBETRAD-1    PIC X(35).                                   
003200*                                 ADRESSRAD BETALNINGSANSVARIG            
003300        05 MOD-ADBETRAD-2    PIC X(35).                                   
003400*                                 ADRESSRAD BETALNINGSANSVARIG            
003500     03 MOD-IDSKYLT-ATTR     PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDSKYLT          PIC X(3).                                    
003800*                                 NATIONALITETSTECKEN                     
003900*                                 SPRÅKIDENTIFIKATION                     
004000     03 MOD-TIRFS-DAT        PIC X(6).                                    
004100     03 MOD-TIRFS-TID        PIC X(4).                                    
004200     03 MOD-TITPO            PIC 9(6).                                    
004300*                                 PLANERAD ORDERDATUM                     
004400     03 MOD-KDTULLVE-ATTR    PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-KDTULLVE         PIC 9.                                       
004700*                                 TYP AV PRIS PÅ TULLFAKTURA              
004800     03 MOD-KDVRINFO-ATTR    PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-KDVRINFO         PIC 9.                                       
005100*                                 PÅVERKAN I VR/DSP SYSTEM                
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 309 BYTES                                 
