000100 01  LINE-W5710302.                                                       
000200*                                 INVENTORY ACS MASTER DEVIATION          
000300*                                 LIST                                    
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600*                                 AFP FORMS RECORD TYPE                   
000700     03 LINE-IDARTNR         PIC Z(8)9.                                   
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 LINE-BEART           PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200*                                 PART DESCRIPTION                        
001300     03 LINE-ADLAGOMR        PIC Z9.                                      
001400*                                 LAGEROMRÅDE                             
001500*                                 AREA                                    
001600     03 LINE-ADGANG          PIC Z9.                                      
001700*                                 GÅNG                                    
001800*                                 AISLE                                   
001900     03 LINE-ADPLATS         PIC Z(4)9.                                   
002000*                                 LAGERPLATSNUMMER                        
002100*                                 LOCATION                                
002200     03 LINE-KVLS            PIC -(7)9.                                   
002300*                                 LAGERSALDO                              
002400*                                 STOCK BALANCE                           
002500     03 LINE-KVCOUNT         PIC Z(8)9.                                   
002600*                                 ANTAL TRÄFFAR                           
002700*                                 NUMBER OF HITS                          
002800     03 LINE-SUAVCOST-DEV    PIC -(8)9.9(2).                              
002900*                                 AVVIKANDE SUMMA                         
003000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003100*                                 DEV SUM AVERAGE COST FOREIGN            
003200*                                 CURRENCY                                
003300     03 LINE-IDCOUNTER-REG   PIC X(20).                                   
003400*                                 REG. AV RÄKNING VID INVENTERING         
003500*                                 PERSON TO REG.COUNT                     
003600     03 LINE-IDACSNR         PIC 9(6).                                    
003700*                                 NR.SERIE FÖR ACS-LISTOR                 
003800*                                 SERIAL NO. FOR ACS REPORTS              
003900*** END OF VILMAII-COPY LENGTH= 108 BYTES                                 
