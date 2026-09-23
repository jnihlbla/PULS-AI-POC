000100 01  W4885802-CTX.                                                        
000200*                                 FIL MED INFO OM VILKA ARTIKLAR          
000300*                                 SOM HAR FÅTT NY PLATS                   
000400*                                 SKICKAS TILL HL PDPN                    
000500     03 ASTERISK1            PIC X.                                       
000600     03 IDARTNR              PIC 9(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 ASTERISK2            PIC X.                                       
000900     03 ADLAGOMR             PIC 9(2).                                    
001000*                                 LAGEROMRÅDE                             
001100     03 ASTERISK3            PIC X.                                       
001200     03 ADGANG               PIC 9(2).                                    
001300*                                 GÅNG                                    
001400     03 ASTERISK4            PIC X.                                       
001500     03 ADPLATS              PIC 9(5).                                    
001600*                                 LAGERPLATSNUMMER                        
001700     03 ASTERISK5            PIC X.                                       
001800     03 KVLS-CDC             PIC 9(7).                                    
001900*                                 LAGERSALDO                              
002000     03 ASTERISK6            PIC X.                                       
002100     03 KVPB-TOT             PIC 9(6)V9(1).                               
002200*                                 PERIODBEHOV (PROGNOS)                   
002300     03 ASTERISK7            PIC X.                                       
002400     03 VLARTNTO             PIC 9(8)V9(1).                               
002500*                                 ARTIKELVOLYM NETTO (CM3)                
002600     03 ASTERISK8            PIC X.                                       
002700     03 FL-ALT-BUFF          PIC X.                                       
002800*                                 ALLMÄN FLAGGA                           
002900     03 ASTERISK9            PIC X.                                       
003000     03 BEART                PIC X(25).                                   
003100*                                 ARTIKELBENÄMNING                        
003200     03 ASTERISK10           PIC X.                                       
003300     03 FILLERX3             PIC X(3).                                    
003400*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
