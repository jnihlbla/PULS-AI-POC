000100 01  W4O28301.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O28301                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-IN           PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDDISTR-UT           PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDKUNDNR-IN          PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 IDKUNDNR-UT          PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 IDARTNR-IN           PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 IDARTNR-UT           PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 IDDC-IN              PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 IDDC-UT              PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 KDORDBEK-IN          PIC X(2).                                    
002500*                                 ORDERBEKRÄFTELSEKOD                     
002600     03 KDORDBEK-UT          PIC X(2).                                    
002700*                                 ORDERBEKRÄFTELSEKOD                     
002800     03 IDORDER-IN           PIC X(7).                                    
002900*                                 ORDERNUMMER                             
003000     03 IDORDER-UT           PIC X(7).                                    
003100*                                 ORDERNUMMER                             
003200     03 KDFRAKT-IN           PIC X(2).                                    
003300*                                 FRAKTSÄTT C1-C2 TILL KUND               
003400     03 KDFRAKT-UT           PIC X(2).                                    
003500*                                 FRAKTSÄTT C1-C2 TILL KUND               
003600     03 KDORDKL-IN           PIC X.                                       
003700*                                 ORDERKLASS                              
003800     03 KDORDKL-UT           PIC X.                                       
003900*                                 ORDERKLASS                              
004000     03 IDORDER-NEXT         PIC 9(7).                                    
004100*                                 VOLVO PARTS ORDERNUMMER                 
004200     03 IDARTNR-NEXT         PIC 9(9).                                    
004300*                                 ARTIKELNUMMER                           
004400     03 IDLOPNR-NEXT         PIC 9(3).                                    
004500*                                 LÖPNUMMER                               
004600     03 IDSEKVNR-NEXT        PIC 9(3).                                    
004700*                                 GENERELLT SEKVENSNUMMER                 
004800     03 IDDC-NEXT            PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000     03 KDORDBEK-NEXT        PIC 9(2).                                    
005100*                                 ORDERBEKRÄFTELSEKOD                     
005200     03 TITIREGD-9KOMPL-NEXT PIC 9(9).                                    
005300*                                 DATUMETS 9-KOMPLEMENT                   
005400     03 IDKUNDRF-NEXT        PIC X(10).                                   
005500*                                 KUNDENS REFERENS (ORDERID)              
005600     03 PROFORMA-ATTR        PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 PROFORMA             PIC X.                                       
005900*                                 ALLMÄN FLAGGA                           
006000     03 PROFORMA-SPAR        PIC X.                                       
006100*                                 ALLMÄN FLAGGA                           
006200     03 KDORDBEK             OCCURS 13 TIMES                              
006300                             PIC X(3).                                    
006400*                                 FRAKTSÄTT C1-C2 TILL KUND               
006500     03 TIREGDAT             OCCURS 13 TIMES                              
006600                             PIC 9(6).                                    
006700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006800     03 IDARTNR              OCCURS 13 TIMES                              
006900                             PIC X(11).                                   
007000*                                 ARTIKELNR+KSIFFRA   IDARTNR-006         
007100     03 IDORDNR7             OCCURS 13 TIMES                              
007200                             PIC Z(6)9.                                   
007300*                                 ORDERNUMMER                             
007400     03 BEKUNDRF             OCCURS 13 TIMES                              
007500                             PIC X(15).                                   
007600*                                 KUNDENS REFERENS                        
007700     03 KVANTAL              OCCURS 13 TIMES                              
007800                             PIC Z(7).                                    
007900*                                 ANTAL ALLMÄNT                           
008000     03 IDDC                 OCCURS 13 TIMES                              
008100                             PIC X(2).                                    
008200*                                 IDENTIFIERARE LAGER                     
008300     03 IDORDNR7-REF         OCCURS 13 TIMES                              
008400                             PIC Z(6)9.                                   
008500*                                 ORDERNUMMER                             
008600     03 TITPO                OCCURS 13 TIMES                              
008700                             PIC 9(6).                                    
008800*                                 PLANERAD ORDERDATUM                     
008900     03 IDORDER-ENTER        PIC 9(7).                                    
009000*                                 VOLVO PARTS ORDERNUMMER                 
009100     03 IDARTNR-ENTER        PIC 9(9).                                    
009200*                                 ARTIKELNUMMER                           
009300     03 IDLOPNR-ENTER        PIC 9(3).                                    
009400*                                 LÖPNUMMER                               
009500     03 IDSEKVNR-ENTER       PIC 9(3).                                    
009600*                                 GENERELLT SEKVENSNUMMER                 
009700     03 IDDC-ENTER           PIC X(2).                                    
009800*                                 IDENTIFIERARE LAGER                     
009900     03 KDORDBEK-ENTER       PIC 9(2).                                    
010000*                                 ORDERBEKRÄFTELSEKOD                     
010100     03 TITIREGD-9KOMPL-ENTER                                             
010200                             PIC 9(9).                                    
010300*                                 DATUMETS 9-KOMPLEMENT                   
010400     03 IDKUNDRF-ENTER       PIC X(10).                                   
010500*                                 KUNDENS REFERENS (ORDERID)              
010600     03 TEMFSINF             PIC X(55).                                   
010700*                                 INFORMATIONSMEDDELANDE                  
010800*** END OF VILMAII-COPY LENGTH= 1091 BYTES                                
