000100 01  RESP-W60330O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0104         
000300*                                 PARTS HELD AT CUSTOMS                   
000400     03 RESP-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600*                                 NUMBER OF LINES                         
000700     03 RESP-W60330O1        OCCURS 500 TIMES.                            
000800*                                 RESPONS-COPYTEXT FÖR PGM W60330         
000900*                                 PARTS HELD AT CUSTOMS                   
001000        05 RESP-IDARTNR      PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300        05 RESP-BEART        PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500*                                 PART DESCRIPTION                        
001600        05 RESP-KVAVIS       PIC Z(5)9.                                   
001700*                                 AVISERAT ANTAL                          
001800*                                 QUANTITY NOTIFIED                       
001900        05 RESP-IDFAKT       PIC Z(6)9.                                   
002000*                                 FAKTURANUMMER                           
002100*                                 INVOICE NO.                             
002200        05 RESP-IDORDNR7     PIC Z(6)9.                                   
002300*                                 ORDERNUMMER                             
002400*                                 ORDER NUMBER                            
002500        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800        05 RESP-IDKOLLI      PIC Z(4)9.                                   
002900*                                 KOLLINUMMER                             
003000*                                 CASE NUMBER                             
003100        05 RESP-DAINLEV      PIC 9(16).                                   
003200*                                 INLEVERANS NUMMER                       
003300*                                 CONSIGNMENT IDENTITY                    
003400*                                 (YYYYMMDD+HHMMSSTH)                     
003500        05 RESP-IDMSG-ERR-LINE                                            
003600                             PIC X(3).                                    
003700*                                 FELMEDDELANDE ID                        
003800*                                 ERROR MESSAGE ID                        
003900        05 RESP-KVANTMOT     PIC X(6).                                    
004000*                                 ANTAL MOTTAGET                          
004100*                                 QUANTITY RECEIVED                       
004200        05 RESP-IDTRACK      PIC X(25).                                   
004300*                                 TRACKING ID FROM CUSTOMS                
004400*                                 CUSTOMS TRACKING ID                     
004500*** END OF VILMAII-COPY LENGTH= 57505 BYTES                               
