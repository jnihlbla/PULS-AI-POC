000100 01  4448-WDGX4448-CTX.                                                   
000200*                                 BESKRIVNING AV                          
000300*                                 PRC TABELLER                            
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 (IDPRC + LOW-VALUE)                     
000600     03 4448-IDPRC.                                                       
000700*                                 PRODUKTIONSKANAL                        
000800*                                 PRODUCTION CHANNEL                      
000900        05 4448-IDPRCBAS     PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100*                                 PRC-BASIC                               
001200        05 4448-IDPRCVAR     PIC X.                                       
001300*                                 PRC-VARIANT                             
001400*                                 PRC-VARIANT                             
001500     03 4448-LOW-VALUE       PIC X.                                       
001600     03 4448-ADLAGOMR        OCCURS 10 TIMES                              
001700                             PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE                             
001900*                                 AREA                                    
002000     03 4448-BEPRC           PIC X(15).                                   
002100*                                 PRODUKTIONKANALSNAMN                    
002200*                                 NAME OF THE PRODUCTION CHANNEL          
002300     03 4448-IDPRC-HUV.                                                   
002400*                                 HUVUDPRODUKTIONSKANAL                   
002500*                                 MAIN PRODUCTION CHANNEL                 
002600        05 4448-IDPRCBAS-HUV PIC X(3).                                    
002700*                                 PRC-BAS                                 
002800*                                 PRC-BASIC                               
002900        05 4448-IDPRCVAR-HUV PIC X.                                       
003000*                                 PRC-VARIANT                             
003100*                                 PRC-VARIANT                             
003200     03 4448-IDPRC-SUB       OCCURS 10 TIMES.                             
003300*                                 PICKUP PRODUKTIONSKANAL                 
003400*                                 PICKUP PRODUCTION CHANNEL               
003500        05 4448-IDPRCBAS-SUB PIC X(3).                                    
003600*                                 PRC-BAS                                 
003700*                                 PRC-BASIC                               
003800        05 4448-IDPRCVAR-SUB PIC X.                                       
003900*                                 PRC-VARIANT                             
004000*                                 PRC-VARIANT                             
004100     03 4448-RESPLIT         PIC S9V9(2)         COMP-3.                  
004200*                                 FAKTOR FÖR PLOCKSATSSTORLEK             
004300*                                 FACTOR FOR PICKING-UNIT SIZE            
004400     03 4448-KDPRCGRP        PIC X(5).                                    
004500*                                 PRODUKTIONSKANALSGRUPP                  
004600*                                 GROUP OF PRODUCTION CHANNELS            
004700     03 4448-KDPRCTYP        PIC X.                                       
004800*                                 PRODUKTIONSKANALSTYP                    
004900*                                 TYPE OF PRODUCTION CHANNEL              
005000     03 4448-KDPRODKL        PIC X.                                       
005100*                                 PRODUKTIONSKLASS                        
005200*                                 PRODUCTION CLASS                        
005300     03 4448-KVARBTID        PIC S9(2)V9(1)      COMP-3.                  
005400*                                 ANTAL MANTIMMAR                         
005500*                                 NUMBER OF MAN HOURS                     
005600     03 4448-KVBEMAN-ORD     PIC S9(2)V9(1)      COMP-3.                  
005700*                                 BEMANNING, KAPACITET ORDINARIE          
005800*                                 AVAILABLE CAPACITY ORDINARY             
005900     03 4448-KVBEMAN-EXT     PIC S9(2)V9(1)      COMP-3.                  
006000*                                 BEMANNING, KAPACITET (EXTRA)            
006100*                                 AVAILABLE CAPACITY (EXTRA)              
006200     03 4448-KVORDER         PIC S9(7)           COMP-3.                  
006300*                                 ANTAL ORDER                             
006400*                                 QUANTITY OF ORDERS                      
006500     03 4448-KVPLSRAD        PIC S9(5)           COMP-3.                  
006600*                                 ANTAL RADER EGEN PLOCKSATS              
006700*                                 NUMBER OF LINES OWN PICKING UNI         
006800*                                 T                                       
006900     03 4448-KVRADER         PIC S9(5)           COMP-3.                  
007000*                                 ANTAL RADER                             
007100*                                 NUMBER OF LINES                         
007200     03 4448-KVVTID          PIC S9(3)V9(2)      COMP-3.                  
007300*                                 ORDER VÄNTETID I PRC (TTMM)             
007400*                                 WAITING TIME IN PRC (HHMM)              
007500     03 4448-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
007600*                                 ORDERVIKT NETTO (KG)                    
007700*                                 WEIGHT PER ORDER NETTO (KG)             
007800     03 4448-VKPLSNTO        PIC S9(6)V9(1)      COMP-3.                  
007900*                                 ORDERVIKT NETTO EGEN PSATS (KG)         
008000*                                 ORDERWEIGHT NET OWN P.UNIT (KG)         
008100     03 4448-VLKOLGR         PIC S9V9(2)         COMP-3.                  
008200*                                 NETTOGRÄNS FÖR EGET KOLLI I M3          
008300*                                 NET LIMIT FOR OWN CASE IN M3            
008400     03 4448-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
008500*                                 ORDERVOLYM NETTO (M3)                   
008600*                                 NET VOLUME PER ORDER (M3)               
008700     03 4448-VLPLSNTO        PIC S9(4)V9(3)      COMP-3.                  
008800*                                 ORDERVOLYM NETTO EGEN PSATS (M3         
008900*                                 )                                       
009000*                                 NET ORDERVOLUME OWN PUNIT (M3)          
009100     03 4448-FLSTJORD        PIC X.                                       
009200*                                 FLAGGA FÖR STJÄRNORDER PBV              
009300*                                 FLAG STARORDERS IN PBV                  
009400     03 4448-FILLERX19       PIC X(19).                                   
009500*** END OF VILMAII-COPY LENGTH= 150 BYTES                                 
