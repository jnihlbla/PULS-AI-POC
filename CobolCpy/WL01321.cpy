000100 01  LINE-WL01321.                                                        
000200*                                 COPYTEXT FOR PICKING LABEL LDC          
000300*                                 PU LINE                                 
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 LINE-ADLAGOMR        PIC 9(2).                                    
000700*                                 LAGEROMRÅDE                             
000800     03 LINE-ADGANG          PIC 9(2).                                    
000900*                                 GÅNG                                    
001000     03 LINE-ADPLATSNR       PIC 9(3).                                    
001100*                                 LAGERPLATSNUMMER                        
001200     03 LINE-ADPLNIV-LEFT    PIC 9.                                       
001300*                                 LAGERPLATSNIVÅ                          
001400     03 LINE-ADPLNIV-RIGHT   PIC 9.                                       
001500*                                 LAGERPLATSNIVÅ                          
001600     03 LINE-BERADREF        PIC X(10).                                   
001700*                                 KUNDENS RADREFERENS                     
001800     03 LINE-BEART           PIC X(15).                                   
001900     03 LINE-FLAKPLOC        PIC X.                                       
002000*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
002100     03 LINE-IDARTNR         PIC Z(7)9.                                   
002200*                                 ARTIKELNUMMER                           
002300     03 LINE-IDBORD          PIC X(3).                                    
002400*                                 PACK-BORD                               
002500     03 LINE-IDDC            PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 LINE-IDDISTR         PIC Z(3)9.                                   
002800*                                 DISTRIKTNUMMER                          
002900     03 LINE-IDKUNDNR        PIC Z(5)9.                                   
003000*                                 KUNDNUMMER                              
003100     03 LINE-IDKUNDRF        PIC X(10).                                   
003200*                                 KUNDENS REFERENS (ORDERID)              
003300     03 LINE-IDLOPNR-ORD     PIC 9(3).                                    
003400*                                 ORDERNS ORDNINGSNUMMER INOM             
003500*                                 EN PLOCKSATS                            
003600     03 LINE-IDLOPNR-PL      PIC 9(3).                                    
003700*                                 PLOCKSATSENS LÖPNUMMER PER PRC          
003800     03 LINE-IDPLKLST        PIC Z(2)9.                                   
003900*                                 PLOCKLISTNUMMER                         
004000     03 LINE-IDPRC.                                                       
004100*                                 PRODUKTIONSKANAL                        
004200        05 LINE-IDPRCBAS     PIC X(3).                                    
004300*                                 PRC-BAS                                 
004400        05 LINE-IDPRCVAR     PIC X.                                       
004500*                                 PRC-VARIANT                             
004600     03 LINE-IDPRODNR        PIC Z(6)9.                                   
004700*                                 PRODUKTIONSNUMMER                       
004800     03 LINE-IDRADNR         PIC Z(3)9.                                   
004900*                                 RADNUMMER                               
005000     03 LINE-IDPSN           PIC 9(3).                                    
005100*                                 PROPER SHIPPING NAME                    
005200     03 LINE-IDSPECEMB       PIC Z(4).                                    
005300*                                 SPECIALEMBALLAGEID                      
005400     03 LINE-IDZON           PIC X(2).                                    
005500*                                 TRANSPORTVÄG (RUTT,ZON)                 
005600     03 LINE-KDARTHNT        PIC Z(5)9.                                   
005700*                                 HANTERINGSKOD                           
005800     03 LINE-KDARTURS        PIC X(2).                                    
005900*                                 ARTIKELURSPRUNGSKOD                     
006000     03 LINE-KDEMBAL         PIC X.                                       
006100*                                 KOD FÖR ATT TALA OM EMBALLAGE-T         
006200*                                 YP                                      
006300     03 LINE-KDFARLIG        PIC 9.                                       
006400*                                 KOD FÖR FARLIGT GODS                    
006500     03 LINE-KDORDKL         PIC 9.                                       
006600*                                 ORDERKLASS                              
006700     03 LINE-KDSORT          PIC X(2).                                    
006800*                                 SORT-KOD                                
006900     03 LINE-KVAVBART        PIC Z(5)9.                                   
007000*                                 AVBOKAT ANTAL ARTIKLAR                  
007100     03 LINE-IDLEVART        PIC X(10).                                   
007200     03 LINE-IDKOLLI         PIC 9(5).                                    
007300*                                 KOLLINUMMER                             
007400     03 LINE-KDKOLLI         PIC X(8).                                    
007500*                                 KOLLIKOD                                
007600     03 LINE-BELAGINS-GRP.                                                
007700*                                 LAGERINSTRUKTIONER                      
007800        05 LINE-BELAGINS-DEL1                                             
007900                             PIC X(60).                                   
008000*                                 DEL AV LAGERINSTRUKTION                 
008100        05 LINE-BELAGINS-DEL2                                             
008200                             PIC X(60).                                   
008300*                                 DEL AV LAGERINSTRUKTION                 
008400     03 LINE-BEFDKRAV        PIC X(40).                                   
008500*                                 FÖRRÅDSDATAKRAV                         
008600*** END OF VILMAII-COPY LENGTH= 313 BYTES                                 
