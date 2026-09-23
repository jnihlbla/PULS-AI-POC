000100 01  W4I28101.                                                            
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W4I28101                                
000400     03 IDDISTR-IN           PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 IDDISTR-UT           PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR-IN          PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDKUNDNR-UT          PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 IDORDNR7-IN          PIC X(7).                                    
001300*                                 ORDERNUMMER                             
001400     03 IDORDNR7-UT          PIC X(7).                                    
001500*                                 ORDERNUMMER                             
001600     03 IDDC-IN              PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 IDDC-UT              PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 IDARTNR-IN           PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 IDARTNR-UT           PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 KDORDBEK-IN          PIC X(2).                                    
002500*                                 ORDERBEKRÄFTELSEKOD                     
002600     03 KDORDBEK-UT          PIC X(2).                                    
002700*                                 ORDERBEKRÄFTELSEKOD                     
002800     03 DATUM-IN             PIC X(6).                                    
002900     03 DATUM-UT             PIC X(6).                                    
003000     03 KDFRAKT-IN           PIC X(2).                                    
003100*                                 FRAKTSÄTT C1-C2 TILL KUND               
003200     03 KDFRAKT-UT           PIC X(2).                                    
003300*                                 FRAKTSÄTT C1-C2 TILL KUND               
003400     03 KDORDKL-IN           PIC X.                                       
003500*                                 ORDERKLASS                              
003600     03 KDORDKL-UT           PIC X.                                       
003700*                                 ORDERKLASS                              
003800     03 IDDISTR-ENTER        PIC 9(4).                                    
003900*                                 DISTRIKTNUMMER                          
004000     03 IDKUNDNR-ENTER       PIC 9(6).                                    
004100*                                 KUNDNUMMER                              
004200     03 IDORDNR7-ENTER       PIC 9(7).                                    
004300*                                 ORDERNUMMER                             
004400     03 IDORDER-ENTER        PIC 9(7).                                    
004500*                                 VOLVO PARTS ORDERNUMMER                 
004600     03 IDARTNR-ENTER        PIC 9(9).                                    
004700*                                 ARTIKELNUMMER                           
004800     03 IDLOPNR-ENTER        PIC 9(3).                                    
004900*                                 LÖPNUMMER                               
005000     03 IDSEKVNR-ENTER       PIC 9(3).                                    
005100*                                 GENERELLT SEKVENSNUMMER                 
005200     03 KDORDBEK-ENTER       PIC 9(2).                                    
005300*                                 ORDERBEKRÄFTELSEKOD                     
005400     03 KDPRT                PIC X(3).                                    
005500*                                 PRINTERKOD                              
005600     03 PROFORMA             PIC X.                                       
005700*                                 ALLMÄN FLAGGA                           
005800     03 PROFORMA-UT          PIC X.                                       
005900*                                 ALLMÄN FLAGGA                           
006000     03 W4I28101-001-GRP     OCCURS 13 TIMES.                             
006100*                                 RADCOPYTEXT FÖR MID                     
006200*                                 W4I28101                                
006300        05 KDCMD             PIC X.                                       
006400*                                 RAD-UPPDATERINGSKOMMANDO                
006500        05 IDKUNDNR          PIC 9(6).                                    
006600*                                 KUNDNUMMER                              
006700        05 IDORDNR7          PIC 9(7).                                    
006800*                                 ORDERNUMMER                             
006900        05 IDDC              PIC X(2).                                    
007000*                                 IDENTIFIERARE LAGER                     
007100        05 KDFRAKT           PIC 9(2).                                    
007200*                                 FRAKTSÄTT C1-C2 TILL KUND               
007300        05 KDORDKL           PIC 9.                                       
007400*                                 ORDERKLASS                              
007500        05 IDORDER           PIC 9(7).                                    
007600*                                 VOLVO PARTS ORDERNUMMER                 
007700        05 IDARTNR           PIC 9(9).                                    
007800*                                 ARTIKELNUMMER                           
007900        05 KDORDBEK          PIC 9(2).                                    
008000*                                 ORDERBEKRÄFTELSEKOD                     
008100     03 IDDISTR-NEXT         PIC 9(4).                                    
008200*                                 DISTRIKTNUMMER                          
008300     03 IDKUNDNR-NEXT        PIC 9(6).                                    
008400*                                 KUNDNUMMER                              
008500     03 IDORDNR7-NEXT        PIC 9(7).                                    
008600*                                 ORDERNUMMER                             
008700     03 IDORDER-NEXT         PIC 9(7).                                    
008800*                                 VOLVO PARTS ORDERNUMMER                 
008900     03 IDARTNR-NEXT         PIC 9(9).                                    
009000*                                 ARTIKELNUMMER                           
009100     03 IDLOPNR-NEXT         PIC 9(3).                                    
009200*                                 LÖPNUMMER                               
009300     03 IDSEKVNR-NEXT        PIC 9(3).                                    
009400*                                 GENERELLT SEKVENSNUMMER                 
009500     03 KDORDBEK-NEXT        PIC 9(2).                                    
009600*                                 ORDERBEKRÄFTELSEKOD                     
009700*** END OF VILMAII-COPY LENGTH= 646 BYTES                                 
