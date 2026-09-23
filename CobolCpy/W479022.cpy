000100 01  W479022.                                                             
000200*                                                                         
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001000*                                 PRODUKTIONSNUMMER                       
001100     03 IDORDER              PIC S9(7)           COMP-3.                  
001200*                                 VOLVO PARTS ORDERNUMMER                 
001300     03 KDCLAGER             PIC S9              COMP-3.                  
001400      88 KDCLAGER-C1         VALUE +1.                                    
001500      88 KDCLAGER-C2         VALUE +2.                                    
001600*                                 CENTRALLAGERKOD                         
001700     03 TIBEGPAC-C1          PIC S9(7)           COMP-3.                  
001800*                                 BEGÄRD PACKNINGSDAG C1 (ÅÅMMDD)         
001900     03 TIBEGPAC-C2          PIC S9(7)           COMP-3.                  
002000*                                 BEGÄRD PACKNINGSDAG C2 (ÅÅMMDD)         
002100     03 TIORDREG             PIC S9(7)           COMP-3.                  
002200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002300     03 IDPLKLST             PIC S9(3)           COMP-3.                  
002400*                                 PLOCKLISTNUMMER                         
002500     03 IDARTNR              PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002800*                                 LAGEROMRÅDE                             
002900     03 FLRESTN              PIC X.                                       
003000      88 FLRESTN-JA          VALUE 'J'.                                   
003100      88 FLRESTN-NEJ         VALUE 'N'.                                   
003200      88 FLRESTN-EJ-IFYLLD   VALUE ' '.                                   
003300*                                 RESTNOTERING ?                          
003400     03 IDLEVNR              PIC S9(5)           COMP-3.                  
003500*                                 LEVERANTÖRNUMMER                        
003600     03 IDKUNDRF-RO          PIC X(10).                                   
003700*                                 KUND REF PÅ RO                          
003800     03 KDCLAGER-LEV         PIC S9              COMP-3.                  
003900      88 KDCLAGER-C1         VALUE +1.                                    
004000      88 KDCLAGER-C2         VALUE +2.                                    
004100*                                 LEVERERANDE C-LAGER                     
004200     03 KDFRAKT              PIC S9(3)           COMP-3.                  
004300*                                 FRAKTSÄTT C1-C2 TILL KUND               
004400     03 KDORDKL              PIC S9              COMP-3.                  
004500      88 KDORDKL-VOR         VALUE +0.                                    
004600      88 KDORDKL-DAG         VALUE +1.                                    
004700      88 KDORDKL-2           VALUE +2.                                    
004800      88 KDORDKL-SNABB       VALUE +2.                                    
004900      88 KDORDKL-SPECIAL     VALUE +3.                                    
005000      88 KDORDKL-KVANT       VALUE +4.                                    
005100      88 KDORDKL-SATS        VALUE +5.                                    
005200*                                 ORDERKLASS                              
005300     03 KDRADSTA             PIC S9              COMP-3.                  
005400*                                 STATUS PÅ ORDERRAD                      
005500     03 KVANNANT             PIC S9(7)           COMP-3.                  
005600*                                 ANNULLERAT ANTAL ARTIKLAR               
005700     03 KVAVBART             PIC S9(7)           COMP-3.                  
005800*                                 AVBOKAT ANTAL ARTIKLAR                  
005900     03 KVBEART              PIC S9(7)           COMP-3.                  
006000*                                 BESTÄLLT ANTAL ARTIKLAR                 
006100     03 KVLEVART             PIC S9(7)           COMP-3.                  
006200*                                 LEVERERAT ANTAL ARTIKLAR                
006300     03 KVORDRAD-C1          PIC S9(5)           COMP-3.                  
006400*                                 ANTAL ORDERRADER C1                     
006500     03 KVORDRAD-C2          PIC S9(5)           COMP-3.                  
006600*                                 ANTAL ORDERRADER C2                     
006700     03 KVORDRAD-PACK        PIC S9(5)           COMP-3.                  
006800*                                 ANTAL PACKADE ORDERRADER                
006900     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
007000*                                 ARTIKELPRIS NETTO                       
007100     03 TIRODAT              PIC S9(7)           COMP-3.                  
007200*                                 RESTORDERDATUM         (ÅÅMMDD)         
007300     03 VKARTNTO             PIC S9(4)V9(3)      COMP-3.                  
007400*                                 ARTIKELVIKT NETTO (KG)                  
007500     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
007600*                                 ARTIKELVOLYM NETTO (CM3)                
007700     03 IDKAMPRF             PIC S9(7)           COMP-3.                  
007800*                                 KAMPANJREFERENS                         
007900     03 IDLOPNR              PIC S9(3)           COMP-3.                  
008000*                                 LÖPNUMMER                               
008100     03 IDSYSTEM             PIC X(4).                                    
008200*                                 SKAPANDE SYSTEMNUMMER                   
008300     03 IDPURAD              PIC S9(5)           COMP-3.                  
008400*                                 RADNUMMER PÅ PACKUNDERLAG               
008500*** END COPY W479022     LENGTH=122                                       
