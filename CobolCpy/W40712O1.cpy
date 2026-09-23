000100 01  RESP-W4O71201.                                                       
000200*                                 RESP-COPYTEXT FÖR W4071200              
000300*                                                                         
000400     03 RESP-IDDISTR         PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 RESP-IDKUNDNR        PIC 9(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 RESP-IDRAPPNR        PIC 9(7).                                    
000900*                                 RAPPORT NUMMER                          
001000     03 RESP-TILEVANM        PIC X(10).                                   
001100*                                 DATUM LEVERANSANMÄRKNING                
001200     03 RESP-TIAAAA-MM-DD REDEFINES RESP-TILEVANM.                        
001300        05 RESP-TIAAAA       PIC 9(4).                                    
001400*                                 ÅRTAL (ÅÅÅÅ)                            
001500        05 RESP-TEHYPHEN     PIC X.                                       
001600*                                 BINDESTRECK                             
001700        05 RESP-TIMM         PIC 9(2).                                    
001800*                                 MÅNAD (MM)                              
001900        05 RESP-TEHYPHEN     PIC X.                                       
002000*                                 BINDESTRECK                             
002100        05 RESP-TIDD         PIC 9(2).                                    
002200*                                 DAG I MÅNAD (DD)                        
002300     03 RESP-KDLEVANM        PIC X.                                       
002400*                                 STATUS LEVERANSANMÄRKNING               
002500     03 RESP-KDVALISO        PIC X(3).                                    
002600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002700     03 RESP-TEANMNOT-DLR    OCCURS 3 TIMES                               
002800                             PIC X(70).                                   
002900*                                 FRI TEXT FRÅN ADM. TILL DEALER          
003000     03 RESP-IDMFSINF        PIC X(3).                                    
003100*                                 MFS INFO. MEDDELANDE NUMMER             
003200     03 RESP-TEMFSINF        PIC X(55).                                   
003300*                                 INFORMATIONSMEDDELANDE                  
003400     03 RESP-KVRADER         PIC 9(3).                                    
003500*                                 ANTAL RADER                             
003600     03 RESP-INFO-RAD        OCCURS 1 TO 999 TIMES                        
003700                             DEPENDING ON RESP-KVRADER.                   
003800*                                 RADINFORMATION                          
003900        05 RESP-IDARTNR      PIC 9(9).                                    
004000*                                 ARTIKELNUMMER                           
004100        05 RESP-IDRADNR      PIC 9(3).                                    
004200*                                 RADNUMMER                               
004300        05 RESP-KDANMORS     PIC X(2).                                    
004400*                                 ORSAK TILL LEVERAN KDANMORS-002         
004500        05 RESP-IDORDNR      PIC 9(5).                                    
004600*                                 ORDERNUMMER UTGÅR PD90                  
004700        05 RESP-IDKOLLI      PIC 9(5).                                    
004800*                                 KOLLINUMMER                             
004900        05 RESP-IDDC         PIC X(2).                                    
005000*                                 IDENTIFIERARE LAGER                     
005100        05 RESP-KVLEVANM     PIC 9(6).                                    
005200*                                 LEVERANSANMÄRKNINGSANTAL                
005300        05 RESP-PRARTBTO     PIC Z(6)9.9(2).                              
005400*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005500        05 RESP-KDFAKTYP     PIC X.                                       
005600*                                 FAKTURATYP                              
005700        05 RESP-IDFAKT       PIC 9(7).                                    
005800*                                 FAKTURANUMMER                           
005900        05 RESP-TIFAKT       PIC X(10).                                   
006000*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
006100        05 RESP-TIAAAA-MM-DD REDEFINES RESP-TIFAKT.                       
006200           07 RESP-TIAAAA    PIC 9(4).                                    
006300*                                 ÅRTAL (ÅÅÅÅ)                            
006400           07 RESP-TEHYPHEN  PIC X.                                       
006500*                                 BINDESTRECK                             
006600           07 RESP-TIMM      PIC 9(2).                                    
006700*                                 MÅNAD (MM)                              
006800           07 RESP-TEHYPHEN  PIC X.                                       
006900*                                 BINDESTRECK                             
007000           07 RESP-TIDD      PIC 9(2).                                    
007100*                                 DAG I MÅNAD (DD)                        
007200        05 RESP-KVRETINL     PIC 9(6).                                    
007300*                                 INLAGT ANTAL VID RETUR                  
007400        05 RESP-KVAVV-KVANT  PIC 9(7).                                    
007500*                                 ANTALSAVVIKELSE KVANTITET               
007600        05 RESP-KVAVV-KVAL   PIC 9(7).                                    
007700*                                 ANTALSAVVIKELSE KVALITET                
007800        05 RESP-KDKREBEH     PIC X(3).                                    
007900*                                 BEHANDLINGSSTATUS                       
008000*** END OF VILMAII-COPY LENGTH= 83219 BYTES                               
