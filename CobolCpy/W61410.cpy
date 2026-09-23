000100 01  W61410.                                                              
000200*                                 COPYTEXT TILL FIL W61410                
000300     03 KDSORT1              PIC S9              COMP-3.                  
000400*                                 SORTERINGSKOD                           
000500     03 IDDC-BEST            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 BEART-ENG            PIC X(25).                                   
000800*                                 ENGELSK ARTIKELBENÄMNING                
000900     03 BEART-SVE            PIC X(25).                                   
001000*                                 SVENSK ARTIKELBENÄMNING                 
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001400*                                 FUNKTIONSGRUPP                          
001500     03 IDDC                 PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 IDUSER-SPKVAL        PIC X(8).                                    
001800*                                 ANVÄNDAR-ID KVALITETSPÄRR               
001900     03 KDLEVSP              PIC S9(3)           COMP-3.                  
002000*                                 SPÄRRKOD LEVERANS                       
002100     03 KVLS                 PIC S9(7)           COMP-3.                  
002200*                                 LAGERSALDO                              
002300     03 KVROS                PIC S9(7)           COMP-3.                  
002400*                                 RESTORDERSALDO                          
002500     03 KVSPARR-KVAL         PIC S9(7)           COMP-3.                  
002600*                                 SPÄRRAT ANTAL KVALITETSFEL              
002700     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002800*                                 ARTIKELSTANDARDPRIS                     
002900     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003100     03 TISPARR-KVAL         PIC 9(6).                                    
003200*                                 SPÄRRAD DATUM KVALITETSFEL              
003300     03 IDLEVNR              PIC X(5).                                    
003400*                                 LEVERANTÖRNUMMER                        
003500*** END OF VILMAII-COPY LENGTH= 106 BYTES                                 
