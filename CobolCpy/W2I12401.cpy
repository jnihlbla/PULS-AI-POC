000100 01  MID-W2I12401.                                                        
000200*                                 COPYTEXT FÖR MID W2I12401               
000300*                                                                         
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-SPARADE-NYCKLAR.                                              
000900*                                       SPARADE NYCKLAR PF8               
001000        05 MID-IDDC-SPAR     PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200        05 MID-IDARTNR-SPAR  PIC 9(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MID-RAD1.                                                         
001500*                                  RAD1                                   
001600*                                                                         
001700        05 MID-KDERSTMP      PIC X.                                       
001800*                                 TEMPORÄR ERSÄTTNINGSREGEL               
001900        05 MID-IDANSK        PIC X(3).                                    
002000*                                 ANSKAFFARNUMMER                         
002100        05 MID-KVLS          PIC X(7).                                    
002200*                                 LAGERSALDO                              
002300        05 MID-KVAKS         PIC X(7).                                    
002400*                                 ANKOMSTSALDO                            
002500        05 MID-KVRO          PIC X(6).                                    
002600*                                 ANTAL RESTNOTERADE ARTIKLAR             
002700        05 MID-SUROBEL       PIC X(7).                                    
002800*                                 RESTORDERVÄRDE STANDARDPRIS             
002900        05 MID-KDERS         PIC X(2).                                    
003000*                                 ERSÄTTNINGSKOD                          
003100     03 MID-RADER            OCCURS 8 TIMES                               
003200                             INDEXED MID-IX-1.                            
003300*                                  RADER                                  
003400*                                                                         
003500        05 MID-IDDC-RADER    PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700        05 MID-KVAKS-PAV-RADER                                            
003800                             PIC X(7).                                    
003900*                                 DEL AV AK PÅ VÄG                        
004000        05 MID-SUAKSV-PAV-RADER                                           
004100                             PIC X(7).                                    
004200*                                 AK PÅ VÄG VÄRDE                         
004300        05 MID-TIAVIDAT-RADER                                             
004400                             PIC X(6).                                    
004500*                                 AVISERINGSDATUM (YYMMDD)                
004600        05 MID-KVLS-RADER    PIC X(7).                                    
004700*                                 LAGERSALDO                              
004800        05 MID-SULV-RADER    PIC X(9).                                    
004900*                                 LAGERVÄRDE                              
005000     03 MID-EOP.                                                          
005100*                                  EOP                                    
005200*                                                                         
005300        05 MID-FLFORTS       PIC X.                                       
005400*                                 FÖRSTA GGN =N, FORSTÄTTNING =J          
005500        05 MID-SULV-TOT      PIC X(9).                                    
005600*                                 LAGERVÄRDE                              
005700        05 MID-IDUSER        PIC X(8).                                    
005800*                                 ANVÄNDARENS SÄKERHETS ID                
005900        05 MID-DAREGDAT      PIC X(6).                                    
006000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006100        05 MID-BEKOM         PIC X(50).                                   
006200*                                 KOMMENTAR/INSTRUKTION                   
006300     03 MID-LOGG             OCCURS 14 TIMES                              
006400                             INDEXED MID-IX-1.                            
006500*                                  LOGG                                   
006600*                                                                         
006700        05 MID-DAREGDAT-LOGG PIC X(6).                                    
006800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006900        05 MID-KDERSTMP-LOGG PIC X.                                       
007000*                                 TEMPORÄR ERSÄTTNINGSREGEL               
007100        05 MID-SULV-LOGG     PIC X(9).                                    
007200*                                 LAGERVÄRDE                              
007300        05 MID-IDUSER-LOGG   PIC X(8).                                    
007400*                                 ANVÄNDARENS SÄKERHETS ID                
007500*** END OF VILMAII-COPY LENGTH= 776 BYTES                                 
