000100 01  WL10102.                                                             
000200*                                 COPYTEXT TILL QUALITY-BLOCK REP         
000300*                                 ORT LDC                                 
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 BEART                PIC X(25).                                   
000700*                                 ARTIKELBENÄMNING                        
000800     03 IDARTNR              PIC Z(7)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 IDFKNGRP             PIC Z(3)9.                                   
001100*                                 FUNKTIONSGRUPP                          
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDUSER-SPKVAL        PIC X(8).                                    
001500*                                 ANVÄNDAR-ID KVALITETSPÄRR               
001600     03 KDLEVSP              PIC Z9.                                      
001700*                                 SPÄRRKOD LEVERANS                       
001800     03 KVLS                 PIC -(7)9.                                   
001900*                                 LAGERSALDO                              
002000     03 KVROS                PIC -(7)9.                                   
002100*                                 RESTORDERSALDO                          
002200     03 KVSPARR-KVAL         PIC Z(6)9.                                   
002300*                                 SPÄRRAT ANTAL KVALITETSFEL              
002400     03 TISPARR-KVAL         PIC 9(6).                                    
002500*                                 SPÄRRAD DATUM KVALITETSFEL              
002600*** END OF VILMAII-COPY LENGTH= 88 BYTES                                  
