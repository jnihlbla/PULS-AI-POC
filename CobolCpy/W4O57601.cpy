000100 01  MOD-W4O57601.                                                        
000200*                                 MOD-COPYTEXT F÷R BILD 4576              
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-KDSTARAD-IN      PIC X.                                       
001700*                                 RADSTATUSKOD                            
001800     03 MOD-KDSTARAD-UT      PIC X.                                       
001900*                                 RADSTATUSKOD                            
002000     03 MOD-TABELLRAD        OCCURS 15 TIMES.                             
002100*                                 GRUPP MED TABELL RADER                  
002200        05 MOD-IDDISTR       PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400        05 MOD-IDKUNDNR      PIC Z(7).                                    
002500*                                 KUNDNUMMER                              
002600        05 MOD-TIRODAT       PIC X(6).                                    
002700*                                 RESTORDERDATUM         (≈≈MMDD)         
002800        05 MOD-KVART         PIC X(7).                                    
002900*                                 ANTAL ARTNR PER BRYTBEGREPP             
003000        05 MOD-KDSTARAD      PIC X.                                       
003100*                                 RADSTATUSKOD                            
003200        05 MOD-IDORDNR5      PIC X(5).                                    
003300*                                 ORDERNUMMER                             
003400        05 MOD-KDORDKL       PIC X.                                       
003500*                                 ORDERKLASS                              
003600        05 MOD-KDRAPRIO      PIC X(3).                                    
003700*                                 PRIORITETSKOD P≈ RADEN                  
003800        05 MOD-TIRES         PIC X(6).                                    
003900*                                 RESERVATIONSDATUM                       
004000        05 MOD-IDKUNDRF-LEV  PIC X(10).                                   
004100*                                 KUND REF P≈ LEVERANSORDERN              
004200     03 MOD-TEMFSINF         PIC X(55).                                   
004300*                                 INFORMATIONSMEDDELANDE                  
004400*** END OF VILMAII-COPY LENGTH= 873 BYTES                                 
