000100 01  MID-W4I21401.                                                        
000200*                                 MID-COPYTEXT FÖR W4I21401               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR-IN       PIC X(5).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER                             
001500     03 MID-BEBET.                                                        
001600*                                 BETALNINGSANSVARIG NAMN                 
001700        05 MID-BEBETRAD-1    PIC X(35).                                   
001800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
001900        05 MID-BEBETRAD-2    PIC X(35).                                   
002000*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002100     03 MID-ADBET.                                                        
002200*                                 BETALNINGSANSVARIG ADRESS               
002300        05 MID-ADBETRAD-1    PIC X(35).                                   
002400*                                 ADRESSRAD BETALNINGSANSVARIG            
002500        05 MID-ADBETRAD-2    PIC X(35).                                   
002600*                                 ADRESSRAD BETALNINGSANSVARIG            
002700     03 MID-IDSKYLT          PIC X(3).                                    
002800      88 MID-GODK-IDSKYLT    VALUE 'D  '                                  
002900                             'E  '                                        
003000                             'F  '                                        
003100                             'GB '                                        
003200                             'I  '                                        
003300                             'NL '                                        
003400                             'P  '                                        
003500                             'S  '                                        
003600                             'SF '                                        
003700                             'USA'.                                       
003800*                                 NATIONALITETSTECKEN                     
003900*                                 SPRÅKIDENTIFIKATION                     
004000     03 MID-KDTULLVE         PIC X.                                       
004100*                                 TYP AV PRIS PÅ TULLFAKTURA              
004200     03 MID-KDVRINFO         PIC X.                                       
004300*                                 PÅVERKAN I VR/DSP SYSTEM                
004400*** END OF VILMAII-COPY LENGTH= 175 BYTES                                 
