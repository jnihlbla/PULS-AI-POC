000100 01  REQU-W30182I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W3018200              
000300*                                                                         
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 REQU-IDSPRAK         PIC X(2).                                    
000700*                                 2-STÄLLIG ISO SPRÅKKOD                  
000800     03 REQU-IDKOLLI-KEY     PIC X(5).                                    
000900*                                 KOLLINUMMER                             
001000     03 REQU-KVRADER         PIC 9(5).                                    
001100*                                 ANTAL RADER                             
001200     03 REQU-NYTT-KOLLI-UPD  PIC X.                                       
001300     03 REQU-KOLLI-VIKT-UPD  PIC X(8).                                    
001400*                                 ORDERVIKT BRUTTO (KG)                   
001500     03 REQU-KOLLI-VOL-UPD   PIC X(8).                                    
001600*                                 ORDERVOLYM BRUTTO (M3)                  
001700     03 REQU-TOT-VIKT-UPD    PIC X(8).                                    
001800*                                 ORDERVIKT BRUTTO (KG)                   
001900     03 REQU-TOT-VOL-UPD     PIC X(8).                                    
002000*                                 ORDERVOLYM BRUTTO (M3)                  
002100     03 REQU-SKAPA-PROFORMA  PIC X.                                       
002200     03 REQU-DEL-IDARTNR-OBJ PIC X(9).                                    
002300*                                 OBJEKTNUMMER                            
002400     03 REQU-DEL-KVANTAL     PIC 9(6).                                    
002500*                                 ANTAL                                   
002600     03 REQU-DEL-IDKOLLI     PIC X(5).                                    
002700*                                 KOLLINUMMER                             
002800     03 REQU-NYRAD           OCCURS 500 TIMES.                            
002900*                                 ARTIKELNUMMER I K-FAKTURA               
003000*                                 CLEARING FLEN BORN JAPAN AUSTRA         
003100*                                 LIEN                                    
003200        05 REQU-IDARTNR-OBJ  PIC X(9).                                    
003300*                                 OBJEKTNUMMER                            
003400        05 REQU-KVANTAL      PIC X(7).                                    
003500*                                 ANTAL                                   
003600*** END OF VILMAII-COPY LENGTH= 8068 BYTES                                
