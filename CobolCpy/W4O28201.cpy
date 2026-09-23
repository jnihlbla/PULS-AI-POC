000100 01  MOD-W4O28201.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O28201                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001700*                                 ORDERNUMMER                             
001800     03 MOD-IDORDNR7-UT      PIC X(7).                                    
001900*                                 ORDERNUMMER                             
002000     03 MOD-IDDC-IN          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOD-IDDC-UT          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MOD-IDARTNR-IN       PIC X(9).                                    
002500*                                 ARTIKELNUMMER                           
002600     03 MOD-IDARTNR-UT       PIC X(9).                                    
002700*                                 ARTIKELNUMMER                           
002800     03 MOD-KDORDBEK-IN      PIC X(2).                                    
002900*                                 ORDERBEKRÄFTELSEKOD                     
003000     03 MOD-KDORDBEK-UT      PIC X(2).                                    
003100*                                 ORDERBEKRÄFTELSEKOD                     
003200     03 MOD-KDFRAKT-IN       PIC X(2).                                    
003300*                                 FRAKTSÄTT DC TILL KUND                  
003400     03 MOD-KDFRAKT-UT       PIC X(2).                                    
003500*                                 FRAKTSÄTT DC TILL KUND                  
003600     03 MOD-KDORDKL-IN       PIC X.                                       
003700*                                 ORDERKLASS                              
003800     03 MOD-KDORDKL-UT       PIC X.                                       
003900*                                 ORDERKLASS                              
004000     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
004100*                                 VOLVO PARTS ORDERNUMMER                 
004200     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
004300*                                 ARTIKELNUMMER                           
004400     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
004500*                                 LÖPNUMMER                               
004600     03 MOD-IDSEKVNR-NEXT    PIC 9(3).                                    
004700*                                 GENERELLT SEKVENSNUMMER                 
004800     03 MOD-IDDC-NEXT        PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000     03 MOD-KDORDBEK-NEXT    PIC 9(2).                                    
005100*                                 ORDERBEKRÄFTELSEKOD                     
005200     03 MOD-IDORDER-ENTER    PIC 9(7).                                    
005300*                                 VOLVO PARTS ORDERNUMMER                 
005400     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
005500*                                 ARTIKELNUMMER                           
005600     03 MOD-IDLOPNR-ENTER    PIC 9(3).                                    
005700*                                 LÖPNUMMER                               
005800     03 MOD-IDSEKVNR-ENTER   PIC 9(3).                                    
005900*                                 GENERELLT SEKVENSNUMMER                 
006000     03 MOD-IDDC-ENTER       PIC X(2).                                    
006100*                                 IDENTIFIERARE LAGER                     
006200     03 MOD-KDORDBEK-ENTER   PIC 9(2).                                    
006300*                                 ORDERBEKRÄFTELSEKOD                     
006400     03 MOD-PROFORMA-ATTR    PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-PROFORMA         PIC X.                                       
006700*                                 ALLMÄN FLAGGA                           
006800     03 MOD-PROFORMA-SPAR    PIC X.                                       
006900*                                 ALLMÄN FLAGGA                           
007000     03 MOD-VARHEAD          PIC X(12).                                   
007100     03 MOD-KDORDBEK         OCCURS 12 TIMES                              
007200                             PIC X(3).                                    
007300*                                 FRAKTSÄTT DC TILL KUND                  
007400     03 MOD-TIREGDAT         OCCURS 12 TIMES                              
007500                             PIC 9(6).                                    
007600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007700     03 MOD-IDARTNR          OCCURS 12 TIMES                              
007800                             PIC X(11).                                   
007900*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
008000     03 MOD-BEART            OCCURS 12 TIMES                              
008100                             PIC X(20).                                   
008200*                                 BENÄMNING            BEART-003          
008300     03 MOD-KVANTAL          OCCURS 12 TIMES                              
008400                             PIC Z(7).                                    
008500*                                 ANTAL                                   
008600     03 MOD-KVQPACK          OCCURS 12 TIMES                              
008700                             PIC Z(4)9.                                   
008800*                                 ANTAL KVANTITETFÖRPACKNINGAR            
008900     03 MOD-IDDC             OCCURS 12 TIMES                              
009000                             PIC X(2).                                    
009100*                                 IDENTIFIERARE LAGER                     
009200     03 MOD-IDORDNR7         OCCURS 12 TIMES                              
009300                             PIC Z(6)9.                                   
009400*                                 ORDERNUMMER                             
009500     03 MOD-TITPO            OCCURS 12 TIMES                              
009600                             PIC 9(6).                                    
009700*                                 PLANERAD ORDERDATUM                     
009800     03 MOD-TEMFSINF         PIC X(55).                                   
009900*                                 INFORMATIONSMEDDELANDE                  
010000*** END OF VILMAII-COPY LENGTH= 1037 BYTES                                
