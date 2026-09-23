000100 01  RESP-PL-WL0134O2.                                                    
000200*                                 COPYTEXT FOR PICKING LABEL LDC          
000300*                                 TOTAL                                   
000400     03 RESP-PL-IDAFPRCD-TOT PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 RESP-PL-IDLOPNR-ORD-TOT                                           
000700                             PIC 9(3).                                    
000800*                                 ORDERNS ORDNINGSNUMMER INOM             
000900*                                 EN PLOCKSATS                            
001000     03 RESP-PL-IDLOPNR-PL-TOT                                            
001100                             PIC 9(3).                                    
001200*                                 PLOCKSATSENS LÖPNUMMER PER PRC          
001300     03 RESP-PL-IDPRC-TOT.                                                
001400*                                 PRODUKTIONSKANAL                        
001500        05 RESP-PL-IDPRCBAS  PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700        05 RESP-PL-IDPRCVAR  PIC X.                                       
001800*                                 PRC-VARIANT                             
001900     03 RESP-PL-KVRADER-MAX1 PIC 9(5).                                    
002000*                                 MAX INDEX KOPPLAT TILL OCCURS N         
002100*                                 EDAN.                                   
002200     03 RESP-PL-IDTRPTNR     PIC 9(3).                                    
002300*                                 TRANSPORTIDENTITET                      
002400     03 RESP-PL-TIRFSDAT     PIC 9(6).                                    
002500*                                 KLART FÖR TRANSPORT ÅÅMMDD              
002600     03 RESP-PL-TIRFSTID     PIC 9(4).                                    
002700*                                 KLART FÖR TRANSPORT (TTMM)              
002800     03 RESP-PL-TABELLRAD    OCCURS 1 TO 1100 TIMES                       
002900                             DEPENDING ON RESP-PL-KVRADER-MAX1.           
003000*                                 GRUPP MED TABELL RADER                  
003100        05 RESP-PL-IDAFPRCD  PIC X(10).                                   
003200*                                 AFP-BLANKETT POSTTYP                    
003300        05 RESP-PL-ADLAGOMR  PIC 9(2).                                    
003400*                                 LAGEROMRÅDE                             
003500        05 RESP-PL-ADGANG    PIC 9(2).                                    
003600*                                 GÅNG                                    
003700        05 RESP-PL-ADPLATS   PIC 9(5).                                    
003800*                                 LAGERPLATSNUMMER                        
003900        05 RESP-PL-BERADREF  PIC X(10).                                   
004000*                                 KUNDENS RADREFERENS                     
004100        05 RESP-PL-BEART     PIC X(15).                                   
004200        05 RESP-PL-FLAKPLOC  PIC X.                                       
004300*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
004400        05 RESP-PL-IDARTNR   PIC 9(9).                                    
004500*                                 ARTIKELNUMMER                           
004600        05 RESP-PL-IDBORD    PIC X(3).                                    
004700*                                 PACK-BORD                               
004800        05 RESP-PL-IDDC      PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000        05 RESP-PL-IDDISTR   PIC 9(4).                                    
005100*                                 DISTRIKTNUMMER                          
005200        05 RESP-PL-IDKUNDNR  PIC 9(6).                                    
005300*                                 KUNDNUMMER                              
005400        05 RESP-PL-IDORDNR7  PIC 9(7).                                    
005500*                                 ORDERNUMMER                             
005600        05 RESP-PL-IDLOPNR-ORD                                            
005700                             PIC 9(3).                                    
005800*                                 ORDERNS ORDNINGSNUMMER INOM             
005900*                                 EN PLOCKSATS                            
006000        05 RESP-PL-IDLOPNR-PL                                             
006100                             PIC 9(3).                                    
006200*                                 PLOCKSATSENS LÖPNUMMER PER PRC          
006300        05 RESP-PL-IDPLKLST  PIC 9(3).                                    
006400*                                 PLOCKLISTNUMMER                         
006500        05 RESP-PL-IDPRC.                                                 
006600*                                 PRODUKTIONSKANAL                        
006700           07 RESP-PL-IDPRCBAS                                            
006800                             PIC X(3).                                    
006900*                                 PRC-BAS                                 
007000           07 RESP-PL-IDPRCVAR                                            
007100                             PIC X.                                       
007200*                                 PRC-VARIANT                             
007300        05 RESP-PL-IDPRODNR  PIC 9(7).                                    
007400*                                 PRODUKTIONSNUMMER                       
007500        05 RESP-PL-IDRADNR   PIC 9(4).                                    
007600*                                 RADNUMMER                               
007700        05 RESP-PL-IDPSN     PIC 9(3).                                    
007800*                                 PROPER SHIPPING NAME                    
007900        05 RESP-PL-IDSPECEMB PIC 9(4).                                    
008000*                                 SPECIALEMBALLAGEID                      
008100        05 RESP-PL-IDZON     PIC X(2).                                    
008200*                                 TRANSPORTVÄG (RUTT,ZON)                 
008300        05 RESP-PL-KDARTHNT  PIC 9(6).                                    
008400*                                 HANTERINGSKOD                           
008500        05 RESP-PL-KDARTURS  PIC X(2).                                    
008600*                                 ARTIKELURSPRUNGSKOD                     
008700        05 RESP-PL-KDEMBAL   PIC X.                                       
008800*                                 KOD FÖR ATT TALA OM EMBALLAGE-T         
008900*                                 YP                                      
009000        05 RESP-PL-KDFARLIG  PIC 9.                                       
009100*                                 KOD FÖR FARLIGT GODS                    
009200        05 RESP-PL-KDORDKL   PIC 9.                                       
009300*                                 ORDERKLASS                              
009400        05 RESP-PL-KDSORT    PIC X(2).                                    
009500*                                 SORT-KOD                                
009600        05 RESP-PL-KVAVBART  PIC 9(6).                                    
009700*                                 AVBOKAT ANTAL ARTIKLAR                  
009800        05 RESP-PL-IDLEVART  PIC X(10).                                   
009900        05 RESP-PL-IDKOLLI   PIC 9(5).                                    
010000*                                 KOLLINUMMER                             
010100        05 RESP-PL-KDKOLLI   PIC X(8).                                    
010200*                                 KOLLIKOD                                
010300        05 RESP-PL-BELAGINS-GRP.                                          
010400*                                 LAGERINSTRUKTIONER                      
010500           07 RESP-PL-BELAGINS-DEL1                                       
010600                             PIC X(60).                                   
010700*                                 DEL AV LAGERINSTRUKTION                 
010800           07 RESP-PL-BELAGINS-DEL2                                       
010900                             PIC X(60).                                   
011000*                                 DEL AV LAGERINSTRUKTION                 
011100        05 RESP-PL-BEFDKRAV  PIC X(40).                                   
011200*                                 FÖRRÅDSDATAKRAV                         
011300*** END OF VILMAII-COPY LENGTH= 342138 BYTES                              
