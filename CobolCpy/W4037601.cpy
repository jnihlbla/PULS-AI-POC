000100 01  LINE-W40376.                                                         
000200*                                 COPYTEXT FOR PICKING LABEL LDC          
000300*                                 PU LINE                                 
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 LINE-ADLAGOMR        PIC 9(2).                                    
000700*                                 LAGEROMR≈DE                             
000800     03 LINE-ADGANG          PIC 9(2).                                    
000900*                                 G≈NG                                    
001000     03 LINE-ADPLATSNR       PIC 9(3).                                    
001100*                                 LAGERPLATSNUMMER                        
001200     03 LINE-ADPLNIV-LEFT    PIC 9.                                       
001300*                                 LAGERPLATSNIV≈                          
001400     03 LINE-ADPLNIV-RIGHT   PIC 9.                                       
001500*                                 LAGERPLATSNIV≈                          
001600     03 LINE-BERADREF        PIC X(10).                                   
001700*                                 KUNDENS RADREFERENS                     
001800     03 LINE-BEART           PIC X(12).                                   
001900     03 LINE-FLAKPLOC        PIC X.                                       
002000*                                 ORDERRADEN SKA PLOCKAS P≈ AK            
002100     03 LINE-IDARTNR         PIC Z(7)9.                                   
002200*                                 ARTIKELNUMMER                           
002300     03 LINE-IDBORD          PIC X(3).                                    
002400*                                 PACK-BORD                               
002500     03 LINE-IDDISTR         PIC Z(3)9.                                   
002600*                                 DISTRIKTNUMMER                          
002700     03 LINE-IDKUNDNR        PIC Z(5)9.                                   
002800*                                 KUNDNUMMER                              
002900     03 LINE-IDORDNR5        PIC Z(4)9.                                   
003000*                                 ORDERNUMMER                             
003100     03 LINE-IDLOPNR-ORD     PIC Z(2)9.                                   
003200*                                 ORDERNS ORDNINGSNUMMER INOM             
003300*                                 EN PLOCKSATS                            
003400     03 LINE-IDLOPNR-PL      PIC 9(3).                                    
003500*                                 PLOCKSATSENS L÷PNUMMER INOM             
003600*                                 PRC-GRUPP                               
003700     03 LINE-IDPLKLST        PIC Z(2)9.                                   
003800*                                 PLOCKLISTNUMMER                         
003900     03 LINE-IDPRC.                                                       
004000*                                 PRODUKTIONSKANAL                        
004100        05 LINE-IDPRCBAS     PIC X(3).                                    
004200*                                 PRC-BAS                                 
004300        05 LINE-IDPRCVAR     PIC X.                                       
004400*                                 PRC-VARIANT                             
004500     03 LINE-IDPRODNR        PIC Z(6)9.                                   
004600*                                 PRODUKTIONSNUMMER                       
004700     03 LINE-IDRADNR         PIC Z(3)9.                                   
004800*                                 RADNUMMER                               
004900     03 LINE-KDARTHNT        PIC Z(5)9.                                   
005000*                                 HANTERINGSKOD                           
005100     03 LINE-KDARTURS        PIC X(2).                                    
005200*                                 ARTIKELURSPRUNGSKOD                     
005300     03 LINE-TIRFSDAT        PIC 9(6).                                    
005400*                                 KLART F÷R TRANSPORT ≈≈MMDD              
005500     03 LINE-KDFARLIG        PIC 9.                                       
005600*                                 KOD F÷R FARLIGT GODS                    
005700     03 LINE-KDORDKL         PIC 9.                                       
005800*                                 ORDERKLASS                              
005900     03 LINE-KDSORT          PIC X(2).                                    
006000*                                 SORT-KOD                                
006100     03 LINE-KVAVBART        PIC Z(5)9.                                   
006200*                                 AVBOKAT ANTAL ARTIKLAR                  
006300     03 LINE-COPY            PIC X(6).                                    
006400     03 LINE-IDLEVART        PIC X(10).                                   
006500*** END OF VILMAII-COPY LENGTH= 132 BYTES                                 
