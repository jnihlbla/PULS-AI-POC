000100 01  REQU-WL0196I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0196             
000300*                                 LDC INBOUND                             
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-FREQPACK-KEY    PIC X.                                       
000800     03 REQU-KVRADER-PACK-KEY                                             
000900                             PIC 9(5).                                    
001000*                                 ANTAL RADER                             
001100*                                 NUMBER OF LINES                         
001200     03 REQU-ADLAGOMR-KEY    PIC 9(2).                                    
001300*                                 LAGEROMRÅDE                             
001400*                                 AREA                                    
001500     03 REQU-ADGANG-FROM-KEY PIC 9(2).                                    
001600*                                 GÅNG                                    
001700*                                 AISLE                                   
001800     03 REQU-ADGANG-TOM-KEY  PIC 9(2).                                    
001900*                                 GÅNG                                    
002000*                                 AISLE                                   
002100     03 REQU-KVRADER-PERIOD-KEY                                           
002200                             PIC 9(5).                                    
002300*                                 ANTAL RADER                             
002400*                                 NUMBER OF LINES                         
002500     03 REQU-KDSORT-KEY      PIC X(2).                                    
002600*                                 SORT-KOD                                
002700*                                 UNIT OF MEASURE                         
002800     03 REQU-KDSORT1-KEY     PIC X.                                       
002900*                                 SORTERINGSKOD                           
003000*                                 CODE FOR SORTING                        
003100     03 REQU-BEART-KEY       PIC X(25).                                   
003200*                                 ARTIKELBENÄMNING                        
003300*                                 PART DESCRIPTION                        
003400     03 REQU-IDSPRAK-KEY     PIC X(3).                                    
003500*                                 NATIONALITETSTECKEN                     
003600*                                 SPRÅKIDENTIFIKATION                     
003700*                                 NATIONALITY SIGN                        
003800*                                 LANGUAGE IDENTIFIER                     
003900     03 REQU-KVRADER         PIC 9(5).                                    
004000*                                 ANTAL RADER                             
004100*                                 NUMBER OF LINES                         
004200*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
