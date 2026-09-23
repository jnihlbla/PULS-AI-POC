000100 01  LINE-W476NAPL.                                                       
000200*                                 COPYTEXT FOR NA PROFORMA                
000300*                                 DETAIL LINE DATA                        
000400*                                 RECORD TYPE = L                         
000500     03 LINE-IDAFPRCD        PIC X(10).                                   
000600*                                 AFP-BLANKETT POSTTYP                    
000700     03 LINE-IDARTNR         PIC Z(7)9.                                   
000800*                                 ARTIKELNUMMER                           
000900     03 LINE-REKSIFFR        PIC X.                                       
001000*                                 KONTROLLSIFFRA                          
001100     03 LINE-BEART           PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300     03 LINE-IDLEVNR-ART     PIC X(5).                                    
001400*                                 LEVERANTÖRNR PÅ ARTIKEL                 
001500     03 LINE-KVLEVART        PIC Z(7).                                    
001600*                                 LEVERERAT ANTAL STYCK                   
001700     03 LINE-PRAVCOST        PIC Z(6)9.9(2).                              
001800*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
001900     03 LINE-SUAVCOST        PIC -(8)9.9(2).                              
002000*                                 SUMMA MEDELVÄRDESKOSTNAD I              
002100*                                 UTL.VALUTA                              
002200     03 LINE-BEARTURS        PIC X(15).                                   
002300*                                 ARTIKELURSPRUNGSLAND BENÄMNING          
002400     03 LINE-IDORDNR5        PIC Z(5).                                    
002500*                                 ORDERNUMMER                             
002600     03 LINE-IDKOLLI         PIC Z(5).                                    
002700*                                 KOLLINUMMER                             
002800*** END OF VILMAII-COPY LENGTH= 103 BYTES                                 
