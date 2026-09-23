000100 01  W4O28101.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O28101                                
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
001600     03 DATUM-IN             PIC X(6).                                    
001700     03 DATUM-UT             PIC X(6).                                    
001800     03 IDDC-IN              PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 IDDC-UT              PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 KDFRAKT-IN           PIC X(2).                                    
002300*                                 FRAKTSÄTT C1-C2 TILL KUND               
002400     03 KDFRAKT-UT           PIC X(2).                                    
002500*                                 FRAKTSÄTT C1-C2 TILL KUND               
002600     03 KDORDKL-IN           PIC X.                                       
002700*                                 ORDERKLASS                              
002800     03 KDORDKL-UT           PIC X.                                       
002900*                                 ORDERKLASS                              
003000     03 IDORDNR7-IN          PIC X(7).                                    
003100*                                 ORDERNUMMER                             
003200     03 IDORDNR7-UT          PIC X(7).                                    
003300*                                 ORDERNUMMER                             
003400     03 IDDISTR-ENTER        PIC 9(4).                                    
003500*                                 DISTRIKTNUMMER                          
003600     03 IDKUNDNR-ENTER       PIC 9(6).                                    
003700*                                 KUNDNUMMER                              
003800     03 IDORDNR7-ENTER       PIC 9(7).                                    
003900*                                 ORDERNUMMER                             
004000     03 IDORDER-ENTER        PIC 9(7).                                    
004100*                                 VOLVO PARTS ORDERNUMMER                 
004200     03 IDARTNR-ENTER        PIC 9(9).                                    
004300*                                 ARTIKELNUMMER                           
004400     03 IDLOPNR-ENTER        PIC 9(3).                                    
004500*                                 LÖPNUMMER                               
004600     03 IDSEKVNR-ENTER       PIC 9(3).                                    
004700*                                 GENERELLT SEKVENSNUMMER                 
004800     03 KDORDBEK-ENTER       PIC 9(2).                                    
004900*                                 ORDERBEKRÄFTELSEKOD                     
005000     03 IDARTNR-IN           PIC X(9).                                    
005100*                                 ARTIKELNUMMER                           
005200     03 IDARTNR-UT           PIC X(9).                                    
005300*                                 ARTIKELNUMMER                           
005400     03 KDPRT-ATTR           PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 KDPRT                PIC X(3).                                    
005700*                                 PRINTERKOD                              
005800     03 KDORDBEK-IN          PIC 9(2).                                    
005900*                                 ORDERBEKRÄFTELSEKOD                     
006000     03 KDORDBEK-UT          PIC 9(2).                                    
006100*                                 ORDERBEKRÄFTELSEKOD                     
006200     03 PROFORMA-ATTR        PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 PROFORMA             PIC X.                                       
006500*                                 ALLMÄN FLAGGA                           
006600     03 PROFORMA-UT          PIC X.                                       
006700*                                 ALLMÄN FLAGGA                           
006800     03 W4O28101-001-GRP     OCCURS 13 TIMES.                             
006900*                                 RADCOPYTEXT FÖR MOD                     
007000*                                 W4O28101-RAD                            
007100        05 KDCMD-ATTR        PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 KDCMD             PIC X.                                       
007400*                                 RAD-UPPDATERINGSKOMMANDO                
007500        05 IDKUNDNR          PIC Z(5)9.                                   
007600*                                 KUNDNUMMER                              
007700        05 IDORDNR7          PIC Z(6)9.                                   
007800*                                 ORDERNUMMER                             
007900        05 BEKUNDRF          PIC X(15).                                   
008000*                                 KUNDENS REFERENS                        
008100        05 IDDC              PIC X(2).                                    
008200*                                 IDENTIFIERARE LAGER                     
008300        05 KDFRAKT           PIC 9(2).                                    
008400*                                 FRAKTSÄTT C1-C2 TILL KUND               
008500        05 KDORDKL           PIC 9.                                       
008600*                                 ORDERKLASS                              
008700        05 TIREGDAT          PIC 9(6).                                    
008800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008900        05 IDORDER           PIC 9(7).                                    
009000*                                 VOLVO PARTS ORDERNUMMER                 
009100        05 IDARTNR           PIC 9(9).                                    
009200*                                 ARTIKELNUMMER                           
009300        05 KDORDBEK          PIC 9(2).                                    
009400*                                 ORDERBEKRÄFTELSEKOD                     
009500     03 IDDISTR-NEXT         PIC 9(4).                                    
009600*                                 DISTRIKTNUMMER                          
009700     03 IDKUNDNR-NEXT        PIC 9(6).                                    
009800*                                 KUNDNUMMER                              
009900     03 IDORDNR7-NEXT        PIC 9(7).                                    
010000*                                 ORDERNUMMER                             
010100     03 IDORDER-NEXT         PIC 9(7).                                    
010200*                                 VOLVO PARTS ORDERNUMMER                 
010300     03 IDARTNR-NEXT         PIC 9(9).                                    
010400*                                 ARTIKELNUMMER                           
010500     03 IDLOPNR-NEXT         PIC 9(3).                                    
010600*                                 LÖPNUMMER                               
010700     03 IDSEKVNR-NEXT        PIC 9(3).                                    
010800*                                 GENERELLT SEKVENSNUMMER                 
010900     03 KDORDBEK-NEXT        PIC 9(2).                                    
011000*                                 ORDERBEKRÄFTELSEKOD                     
011100     03 TEMFSINF             PIC X(55).                                   
011200*                                 INFORMATIONSMEDDELANDE                  
011300*** END OF VILMAII-COPY LENGTH= 1048 BYTES                                
