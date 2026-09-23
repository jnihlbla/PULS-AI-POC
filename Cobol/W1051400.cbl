000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1051400.                                                
000400 AUTHOR.         SUSANNE ENEGARD.                                         
000500 DATE-WRITTEN.   JANUARI 1985.                                            
000510 DATE-COMPILED.                                                           
000520     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMETS FRÅGEDELEN HÄMTAR INFORMATION OM KATALOG-            
001100*                    AVSNITTS-HUVUD                                       
001200*        PROGRAMMETS UPPDATERINGSDEL NYREGISTRERAR OCH ÄNDRAR             
001300*                    KATALOGAVSNITTS-HUVUD.                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W1T514                                              
002500*        MID:         W1I51401                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W1O51401                                            
002900*                                                                         
002910*                                                                         
002920*    SCR:                                                                 
002930*        991029  Illustration får bara finnas på 0001-avsnitt             
002940*                om det är en huvudgrupp (*0).                            
002941*        991122  Åtgärdar "fällan" NOT+OR i DA- sektionen. (oops)         
002950*                                                                         
003000****************************************************************          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP3                                                                
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W1051400'.            
003800 01  FILLER                      PIC X(16)   VALUE                        
003801                                            'FELTEXT '.                   
003802 01  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003803 01  ABENDINFO                   PIC X(80)   VALUE SPACE.                 
003810 01  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
003910 77  OCH                         PIC X       VALUE '&'.                   
004000 77  INDX                        PIC S9(9)   VALUE +1   COMP SYNC.        
004100 77  GRP-IX                      PIC S9(9)   VALUE +1   COMP SYNC.        
004200 77  SPRAAK-IX                   PIC S9(9)   VALUE +1   COMP SYNC.        
004300 77  PER-IX                      PIC S9(9)   VALUE ZERO COMP SYNC.        
004400 77  PER-IX-MAX                  PIC S9(9)   VALUE +12  COMP SYNC.        
004410 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
004500 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +600 COMP SYNC.        
004600 77  INDATA-FEL                  PIC X(1)    VALUE 'N'.                   
004700 77  NYCKLAR-FEL                 PIC X(1)    VALUE 'N'.                   
004800 77  UPPDATE-FLAGGA              PIC X(1)    VALUE 'N'.                   
004900 77  NYUPPLAGG-NOLLRAD           PIC X(1)    VALUE 'N'.                   
005000 77  ROT-FINNS                   PIC X(1)    VALUE 'N'.                   
005100 77  IDILLU-FINNS                PIC X(1)    VALUE 'N'.                   
005200 77  GALLANDE-ILLU-FINNS         PIC X(1)    VALUE 'N'.                   
005300 77  IDRUBNR-FINNS               PIC X(1)    VALUE 'N'.                   
005400 77  BERUBTEXT-FINNS             PIC X(1)    VALUE 'N'.                   
005500 77  IDFOTNR-RAD4-FINNS          PIC X(1)    VALUE 'N'.                   
005600 77  IDFOTNR-FINNS               PIC X(1)    VALUE 'N'.                   
005700 77  TEKOL-FINNS                 PIC X(1)    VALUE 'N'.                   
005800 77  GODK-KATPUB-FINNS           PIC X(1)    VALUE 'N'.                   
005900 77  UPPDAT-TEKOL                PIC X(1)    VALUE 'N'.                   
006000 77  UPPDAT-IDFOTNR              PIC X(1)    VALUE 'N'.                   
006100 77  GALLANDE-ILLU               PIC X(1)    VALUE 'N'.                   
006200 77  GALLANDE-FINNS              PIC X(1)    VALUE 'N'.                   
006300 77  NASTA-FINNS                 PIC X(1)    VALUE 'N'.                   
006400 77  RADER-FINNS                 PIC X(1)    VALUE 'N'.                   
006500 77  TAB-FINNS                   PIC X(1)    VALUE 'N'.                   
006600 77  RAD-DATUM-FEL               PIC X(1)    VALUE 'N'.                   
006700 77  AVS-UTGANGSMARKERAT         PIC X(1)    VALUE 'N'.                   
006800 77  WS-BORT                     PIC X(1)    VALUE SPACE.                 
006900 77  GENERAL-KOLL-TOM            PIC X       VALUE SPACE.                 
007000 77  GENERAL-KOLL-FROM           PIC X       VALUE SPACE.                 
007100 77  SW-BORTTAG                  PIC X(1)    VALUE 'N'.                   
007200 77  SW-KOPIERING                PIC X(1)    VALUE 'N'.                   
007300 77  SW-NYUPPLAGG                PIC X(1)    VALUE 'N'.                   
007400 77  SW-UPPDATERING              PIC X(1)    VALUE 'N'.                   
007500 77  SW-NYUPPLAGG-NYTT           PIC X(1)    VALUE 'N'.                   
007600 77  SW-KDCATPUB-FROM            PIC X(1)    VALUE 'N'.                   
007700 77  SW-KDCATPUB-TOM             PIC X(1)    VALUE 'N'.                   
007800 77  SW-OMRAKN-FROM              PIC X(1)    VALUE 'N'.                   
007900 77  SPAR-OMRAKN-FROM            PIC X(1)    VALUE 'N'.                   
008000 77  SW-OMRAKN-TOM               PIC X(1)    VALUE 'N'.                   
008100 77  SPAR-IDILLU                 PIC S9(5)   VALUE ZERO COMP-3.           
008200 77  TEST-IX                     PIC S9(5)   VALUE ZERO COMP-3.           
008300 77  NYUPPLAGG-IX                PIC S9(5)   VALUE ZERO COMP-3.           
008400 77  UPPDAT-FROM-IX              PIC S9(5)   VALUE ZERO COMP-3.           
008500 77  UPPDAT-TOM-IX               PIC S9(5)   VALUE ZERO COMP-3.           
008600 77  TAB-IX                      PIC S9(5)   VALUE ZERO COMP-3.           
008700 77  TAB-IX2                     PIC S9(5)   VALUE ZERO COMP-3.           
008800 77  TAB-IX-MAX                  PIC S9(5)   VALUE +99  COMP-3.           
008900                                                                          
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
009500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
009600                                                                          
009700 01  TABENTRY-PARM.                                                       
009800     03  STEGLAANGD              PIC S9(9) COMP.                          
009900     03  ANTAL                   PIC S9(9) COMP.                          
010000     03  NYCKELLAANGD            PIC S9(9) COMP.                          
010100                                                                          
010200 01  TABELL.                                                              
010300     03  TAB1-RAD OCCURS 99.                                              
010400        05  TAB1-KDCATPUB-FROM    PIC X(6).                               
010500        05  TAB1-KDCATPUB-TOM     PIC X(6).                               
010600     EJECT                                                                
010700 01  TABELL2.                                                             
010800     03  TAB2-RAD OCCURS 198.                                             
010900        05  TAB2-KDCATPUB-FROM   PIC X(6).                                
011000        05  TAB2-KDCATPUB-TOM    PIC X(6).                                
011100                                                                          
011200 01  IDCATNR-WS                  PIC X(5)    VALUE SPACE.                 
011300 01  FILLER             REDEFINES IDCATNR-WS.                             
011400     03  KEY-IDCATNR             PIC 9(5).                                
011500 01  IDCATGRP-WS                 PIC X(2)    VALUE SPACE.                 
011600 01  FILLER             REDEFINES IDCATGRP-WS.                            
011700     03  KEY-IDCATGRP            PIC 9(2).                                
011800 01  IDCATAVS-WS                 PIC X(4)    VALUE SPACE.                 
011900 01  FILLER             REDEFINES IDCATAVS-WS.                            
012000     03  KEY-IDCATAVS            PIC 9(4).                                
012100 01  KEY-IDCATRAD-X              PIC X(4)    VALUE SPACE.                 
012200 01  FILLER             REDEFINES KEY-IDCATRAD-X.                         
012300     03  KEY-IDCATRAD-NUM        PIC 9(4).                                
012400 01  WS-KDCATPUB                 PIC X(6).                                
012500 01  WS-KDCATPUB-FROM            PIC X(6).                                
012600 01  WS-KDCATPUB-TOM             PIC X(6).                                
012700 01  HANVISN-KDCATPUB-TOM        PIC X(6).                                
012800 01  SPAR-KDCATPUB               PIC X(6).                                
012900 01  LAS-KDCATPUB                PIC X(6).                                
013000 01  WS-KDCATPUB-NEXT            PIC X(6).                                
013100 01  GALLANDE-KDCATPUB           PIC X(6).                                
013200 01  WS-KDCATPUB-COPY            PIC X(6).                                
013300 01  TEST-KDCATPUB-FROM          PIC X(6).                                
013400 01  KDCATPUB-FROM-TEST REDEFINES TEST-KDCATPUB-FROM  PIC 9(6).           
013500 01  TEST-KDCATPUB-TOM           PIC X(6).                                
013600 01  KDCATPUB-TOM-TEST REDEFINES TEST-KDCATPUB-TOM    PIC 9(6).           
013700 01  TEST-KDCATPUB-COPY          PIC X(6).                                
013800 01  KDCATPUB-NEW                PIC X(6).                                
013900 01  KDCATPUB-OLD                PIC X(6).                                
014000 01  SPAR-KDCATPUB-NEW           PIC X(6).                                
014100 01  SPAR-KDCATPUB-OLD           PIC X(6).                                
014200 77  FIXAD-KDCATPUB-TOM          PIC X(6).                                
014300                                                                          
014400 01  WS-OMMARKN-TID              PIC 9(3).                                
014500 01  FILLER REDEFINES WS-OMMARKN-TID.                                     
014600     03  WS-OMMARKN-AAR          PIC 9(1).                                
014700     03  WS-OMMARKN-VECKA        PIC 9(2).                                
015200                                                                          
015300******** VECKOADD                                                         
015400 01  W009VADD-DATUM              PIC S9(5) COMP-3.                        
015500 01  W009VADD-ANTAL              PIC S9(3) COMP-3.                        
015600                                                                          
015700 01  VARIABLER.                                                           
015800                                                                          
015900     03  WS-IDSKYLT              PIC X(3)    VALUE SPACE.                 
016000     03  FILLER       OCCURS 3.                                           
016100         05  SPAR-IDRUBNR        PIC 9(5).                                
016200     03  FILLER       OCCURS 3.                                           
016300         05  BAS-IDRUBNR         PIC 9(5).                                
016400     03  FILLER       OCCURS 3.                                           
016500         05  INBAS-IDRUBNR       PIC 9(5).                                
016600     03  FILLER       OCCURS 5.                                           
016700         05  IN-IDRUBNR          PIC 9(5).                                
016800     03  FILLER       OCCURS 5.                                           
016900         05  TEST-IDRUBNR        PIC 9(5).                                
017000     03  FILLER       OCCURS 3.                                           
017100         05  BAS-IDFOTNR-RAD4    PIC 9(5).                                
017200     03  FILLER       OCCURS 3.                                           
017300         05  IN-IDFOTNR-RAD4     PIC 9(5).                                
017400     03  FILLER       OCCURS 3.                                           
017500         05  TEST-IDFOTNR-RAD4   PIC 9(5).                                
017600     03  BAS-IDFOTNR-GRP  OCCURS 5.                                       
017700         05  BAS-IDFOTNR-RAD  OCCURS 3.                                   
017800             07  BAS-IDFOTNR         PIC 9(5).                            
017900     03  TEST-IDFOTNR-GRP OCCURS 5.                                       
018000         05  TEST-IDFOTNR-RAD OCCURS 3.                                   
018100             07  TEST-IDFOTNR        PIC 9(5).                            
018200                                                                          
018300     03  BAS-FLRUBTYP            PIC X       VALUE SPACE.                 
018400     03  IN-FLRUBTYP             PIC X       VALUE SPACE.                 
018500     03  TEST-FLRUBTYP           PIC X       VALUE SPACE.                 
018600     03  DAGENS-DATUM            PIC 9(6)    VALUE ZERO.                  
018700                                                                          
018800     03  DAGENS-AAR-VECKA        PIC 9(4).                                
018900     03  FILLER REDEFINES DAGENS-AAR-VECKA.                               
019000         05  DAGENS-AAR-FORSTA   PIC 9.                                   
019100         05  DAGENS-VECKA        PIC 9(3).                                
019200         05  FILLER REDEFINES DAGENS-VECKA.                               
019300             07 DAGENS-AAR-ANDRA PIC 9.                                   
019400             07 DAGENS-V-VECKA   PIC 9(2).                                
019500                                                                          
019600     03  AKTUELLT-AAR            PIC 9(4).                                
019700     03  FILLER REDEFINES AKTUELLT-AAR.                                   
019800         05  AKTUELLT-AAR-POS1   PIC 9.                                   
019900         05  AKTUELLT-AAR-POS2-VECKA PIC 9(3).                            
020000         05  FILLER REDEFINES AKTUELLT-AAR-POS2-VECKA.                    
020100           07  AKTUELLT-AAR-POS2   PIC 9.                                 
020200           07  AKTUELLT-AAR-VECKA  PIC 9(2).                              
020300                                                                          
020310     03 DAGENS-VECKAS-KATPUB.                                             
020311         05 DAGENS-AAAA          PIC 9(4)    VALUE ZERO.                  
020312         05 DAGENS-VV            PIC 9(2)    VALUE ZERO.                  
020313                                                                          
020320 01  WS-KDCATPUB-R-AVV           PIC X(3)    VALUE SPACE.                 
020330 01  WS-KDCATPUB-AAAAVV          PIC X(6)    VALUE SPACE.                 
020350 01  WS-GILTIGA-AAR.                                                      
020360   03 WS-TIAAAA                  PIC 9(4)    VALUE ZERO                   
020370                                 OCCURS 4.                                
020380                                                                          
020400 01  GEMEN                       PIC X(29)                                
020500     VALUE 'abcdefghijklmnopqrstuvwxyzåäö'.                               
020600 01  VERSAL                      PIC X(29)                                
020700     VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ'.                               
020800                                                                          
020900     EJECT                                                                
021000* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
021100 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-T-DLI'.        
021200 01  NYCKLAR-TILL-DLI.                                                    
021300   03  W-IDCATNR-X.                                                       
021400       05  W-IDCATNR-WDN1        PIC 9(5)    VALUE ZERO.                  
021500   03  W-WDN501KY-X.                                                      
021600       05  W-IDCATNR             PIC 9(5)   VALUE ZERO.                   
021700       05  W-IDCATGRP            PIC 9(2)   VALUE ZERO.                   
021800       05  W-IDCATAVS            PIC 9(4)   VALUE ZERO.                   
021900   03  W-WDN501KY-H-X.                                                    
022000       05  W-IDCATNR-H           PIC 9(5)   VALUE ZERO.                   
022100       05  W-IDCATGRP-H          PIC 9(2)   VALUE ZERO.                   
022200       05  W-IDCATAVS-H          PIC 9(4)   VALUE ZERO.                   
022300   03  W-WDN512KY-X.                                                      
022400       05  W-IDCATRAD            PIC 9(4)   VALUE ZERO.                   
022500       05  W-KDCATPUB            PIC X(6)   VALUE SPACE.                  
022600   03  W-WDN512KY-H-X.                                                    
022700       05  W-IDCATRAD-H          PIC 9(4)   VALUE ZERO.                   
022800       05  W-KDCATPUB-H          PIC X(6)   VALUE SPACE.                  
022900   03  W-WDN512KY-MIN.                                                    
023000       05  W-IDCATRAD-MIN        PIC 9(4)   VALUE ZERO.                   
023100       05  W-KDCATPUB-MIN        PIC X(6)   VALUE LOW-VALUE.              
023200   03  W-WDN512KY-MAX.                                                    
023300       05  W-IDCATRAD-MAX        PIC 9(4)   VALUE ZERO.                   
023400       05  W-KDCATPUB-MAX        PIC X(6)   VALUE HIGH-VALUE.             
023410   03  W-WDN513KY.                                                        
023420       05  W-IDRADNR             PIC S9(3)  VALUE ZERO   COMP-3.          
023430       05  W-KDCATPUB-F13        PIC X(6)   VALUE LOW-VALUE.              
023441                                                                          
023442   03 W-WDN5G1KY-X.                                                       
023443     05 W-WDN5G1KY-HAEN.                                                  
023444*        --- Hänvisat avsnitts RAD-ADRESS                                 
023445        07 W-WDN5GSEQ-01-X.                                               
023446          09 W-IDCATNR-GSEQ     PIC 9(5)   VALUE ZERO.                    
023447          09 W-IDCATGRP-GSEQ    PIC 9(2)   VALUE ZERO.                    
023448          09 W-IDCATAVS-GSEQ    PIC 9(4)   VALUE ZERO.                    
023449        07 W-WDN5GSEQ-12-X.                                               
023450          09 W-IDCATRAD-GSEQ    PIC 9(4)   VALUE ZERO.                    
023451          09 W-KDCATPUB-GSEQ    PIC X(6)   VALUE SPACE.                   
023452     05 W-IDWDN512-REF-X.                                                 
023453*        --- Hänvisande avsnittets RAD-ADRESS                             
023454*        --- FIELD NAME 'IDCATRKY' i det fysiska DBD:t WDN5G              
023455         07 W-IDCATNR-GSEQ-REF   PIC 9(5)   VALUE ZERO.                   
023456         07 W-IDCATGRP-GSEQ-REF  PIC 9(2)   VALUE ZERO.                   
023457         07 W-IDCATAVS-GSEQ-REF  PIC 9(4)   VALUE ZERO.                   
023458         07 W-IDCATRAD-GSEQ-REF  PIC 9(4)   VALUE ZERO.                   
023459         07 W-KDCATPUB-GSEQ-REF  PIC X(6)   VALUE SPACE.                  
023460                                                                          
023461   03  W-IDCATRKY-LO             PIC X(21) VALUE LOW-VALUE.               
023462   03  W-IDCATRKY-HI             PIC X(21) VALUE HIGH-VALUE.              
023470                                                                          
023500   03  W-IDILLU-X.                                                        
023600       05  W-IDILLU              PIC S9(5)   VALUE ZERO  COMP-3.          
023700   03  W-IDRUBNR-X.                                                       
023800       05  W-IDRUBNR             PIC S9(5)   VALUE ZERO  COMP-3.          
023900   03  W-IDFOTNR-X.                                                       
024000       05  W-IDFOTNR             PIC S9(5)   VALUE ZERO  COMP-3.          
024100   03  W-IDSEGMNR-X.                                                      
024200       05  W-IDSEGMNR            PIC S9(1)   VALUE ZERO  COMP-3.          
024300   03  W-IDSKYLT-X.                                                       
024400       05  W-IDSKYLT             PIC X(3)    VALUE SPACE.                 
024500   03  W-TIAAAA-X.                                                        
024600       05  W-TIAAAA              PIC 9(4)    VALUE ZERO.                  
024700     EJECT                                                                
024800* - - - - - - - - - - - - - - - - - - -  MEDDELANDEN                      
024900 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
025000 01  MEDDELANDEN.                                                         
025100     03 FILLER-1.                                                         
025200          05 FILLER              PIC X(22)                                
025300              VALUE '    NYCKEL EJ NUMER1SK'.                             
025400          05 FILLER              PIC X(22)                                
025500              VALUE '    KEY NOT NUMERIC   '.                             
025600     03 FILLER REDEFINES FILLER-1.                                        
025700          05 FEL-1   OCCURS 2    PIC X(22).                               
025800                                                                          
025900     03 FILLER-2.                                                         
026000          05 FILLER              PIC X(23)                                
026100              VALUE '    MARKERADE FÄLT FEL '.                            
026200          05 FILLER              PIC X(23)                                
026300              VALUE '    MARKED FIELDS WRONG'.                            
026400     03 FILLER REDEFINES FILLER-2.                                        
026500          05 FEL-2   OCCURS 2    PIC X(23).                               
026600                                                                          
026700     03 FILLER-3.                                                         
026800          05 FILLER              PIC X(19)                                
026900              VALUE '    NYCKEL FELAKTIG'.                                
027000          05 FILLER              PIC X(19)                                
027100              VALUE '    WRONG KEY      '.                                
027200     03 FILLER REDEFINES FILLER-3.                                        
027300          05 FEL-3   OCCURS 2    PIC X(19).                               
027400                                                                          
027500     03 FILLER-4.                                                         
027600          05 FILLER              PIC X(28)                                
027700              VALUE '    KATALOGNR EJ REGISTRERAT'.                       
027800          05 FILLER              PIC X(28)                                
027900              VALUE '    CATALOGUENO. NOT FOUND  '.                       
028000     03 FILLER REDEFINES FILLER-4.                                        
028100          05 FEL-4   OCCURS 2    PIC X(28).                               
028200                                                                          
028300     03 FILLER-6.                                                         
028400          05 FILLER              PIC X(26)                                
028500              VALUE '    RADER FINNS EJ        '.                         
028600          05 FILLER              PIC X(26)                                
028700              VALUE '   NO LINES ARE REGISTERED'.                         
028800     03 FILLER REDEFINES FILLER-6.                                        
028900          05 FEL-6   OCCURS 2    PIC X(26).                               
029000                                                                          
029100     03 FILLER-7.                                                         
029200          05 FILLER              PIC X(30)                                
029300              VALUE '    GRUPP FÅR INTE VARA NOLL  '.                     
029400          05 FILLER              PIC X(30)                                
029500              VALUE '    GROUP ZERO IS  NOT ALLOWED'.                     
029600     03 FILLER REDEFINES FILLER-7.                                        
029700          05 FEL-7   OCCURS 2    PIC X(30).                               
029800                                                                          
029900     03 FILLER-8.                                                         
030000          05 FILLER              PIC X(31)                                
030100              VALUE '    GRUPP FÅR INTE VARA 10 - 19'.                    
030200          05 FILLER              PIC X(31)                                
030300              VALUE '    GROUPS 10 TO 19 NOT ALLOWED'.                    
030400     03 FILLER REDEFINES FILLER-8.                                        
030500          05 FEL-8   OCCURS 2    PIC X(31).                               
030600                                                                          
030700     03 FILLER-11.                                                        
030800          05 FILLER              PIC X(20)                                
030900              VALUE 'UPPDATERING GJORD.  '.                               
031000          05 FILLER              PIC X(20)                                
031100              VALUE 'DATABASE IS UPDATED.'.                               
031200     03 FILLER REDEFINES FILLER-11.                                       
031300          05 MED-1   OCCURS 2    PIC X(20).                               
031400                                                                          
031500     03 FILLER-12.                                                        
031600          05 FILLER              PIC X(34)                                
031700              VALUE ' REFERENS TILL AVSNITTET, SE 1513 '.                 
031800          05 FILLER              PIC X(34)                                
031900              VALUE ' REF. TO THE TEXT-BLOCK, SEE 1513 '.                 
032000     03 FILLER REDEFINES FILLER-12.                                       
032100          05 MED-2   OCCURS 2    PIC X(34).                               
032200                                                                          
032300     03 FILLER-14.                                                        
032400          05 FILLER              PIC X(21)                                
032500              VALUE ' FLER PUBKODER FINNS '.                              
032600          05 FILLER              PIC X(21)                                
032700              VALUE ' MORE PUB-CODES EXIST'.                              
032800     03 FILLER REDEFINES FILLER-14.                                       
032900          05 MED-3   OCCURS 2    PIC X(21).                               
033000                                                                          
033100     03 FILLER-18.                                                        
033200          05 FILLER              PIC X(34)                                
033300              VALUE ' GILTIGA RADER SAKNAS             '.                 
033400          05 FILLER              PIC X(34)                                
033500              VALUE ' NO VALID LINES ARE REGISTERED    '.                 
033600     03 FILLER REDEFINES FILLER-18.                                       
033700          05 FEL-19  OCCURS 2    PIC X(34).                               
033800                                                                          
033900     03 FILLER-19.                                                        
034000          05 FILLER              PIC X(40)                                
034100          VALUE ' ILLU och VADIS nycklar kopierade. KOLLA'.               
034200          05 FILLER              PIC X(40)                                
034300          VALUE ' ILLU and VADIS keys copied. CHECK OUT  '.               
034400     03 FILLER REDEFINES FILLER-19.                                       
034500          05 MED-7   OCCURS 2    PIC X(40).                               
034600                                                                          
034700     03 FILLER-20.                                                        
034800          05 FILLER              PIC X(34)                                
034900              VALUE ' PUB-KODER STÄMMER INTE -KOLLA    '.                 
035000          05 FILLER              PIC X(34)                                
035100              VALUE ' PLEASE CHECK ALL PUB-CODES       '.                 
035200     03 FILLER REDEFINES FILLER-20.                                       
035300          05 MED-8   OCCURS 2    PIC X(34).                               
035400                                                                          
035500     03 FILLER-21.                                                        
035600          05 FILLER              PIC X(40)                                
035700              VALUE 'UPPDATERING GJORD - KOLLA PUBKODER      '.           
035800          05 FILLER              PIC X(40)                                
035900              VALUE 'UPDATED - PLEASE CHECK THE PUB-CODES    '.           
036000     03 FILLER REDEFINES FILLER-21.                                       
036100          05 MED-9   OCCURS 2    PIC X(40).                               
036200                                                                          
036300     03 FILLER-F1.                                                        
036400          05 FILLER              PIC X(40)                                
036500              VALUE 'SÖKBEGREPP FÖR VADIS ??     1515-BILD   '.           
036600          05 FILLER              PIC X(40)                                
036700              VALUE 'SEARCH ARGUMENTS VADIS ??    SCREEN 1515'.           
036800     03 FILLER REDEFINES FILLER-F1.                                       
036900          05 FRAGA-1   OCCURS 2    PIC X(40).                             
037000     EJECT                                                                
037100* - - - - - - - - - - - - - - - - - - - - DAT-AREA                        
037200 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
037300*01  -COPY WDATAREA                                                       
037400     EJECT                                                                
037500* - - - - - - - - - - - - - - - - - - - - LAND-AREA                       
037600*** 01  FILLER                   PIC X(16)   VALUE 'LAND-AREA'.           
037700*** 01  -COPY WWLAND03                                                    
037800*    EJECT                                                                
037900* - - - - - - - - - - - - - - - - - - - - MID-AREA                        
038000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
038100*01  -COPY W1I51401.                                                      
038200     EJECT                                                                
038300* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
038400 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
038500*01  -COPY WMSGAREA                                                       
038600     EJECT                                                                
038700*    03  POST  -COPY W1O51401 -RED MSG-AREA.                              
038800     EJECT                                                                
038900* - - - - - - - - - - - - - - - - - - -  MFS-AREA                         
039000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
039100*01  -COPY WMFSAREA                                                       
039200     EJECT                                                                
039300* - - - - - - - - - - - - - - - - - - -  IMS-WS                           
039400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039500 01  IMS-WS.                                                              
039600*                        **** STATUS-KOD FRÅN IMS                         
039700   03  STATUS-WS                 PIC XX.                                  
039800     88  SEGMENT-FINNS                       VALUE '  '.                  
039900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040000     88  BASEN-SLUT                          VALUE 'GB'.                  
040100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
040200     SKIP2                                                                
040300   03  GODK-STATUSKODER.                                                  
040400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040500     SKIP2                                                                
040600 01  SSA1                        PIC X(128).                              
040700 01  SSA2                        PIC X(128).                              
040800     EJECT                                                                
040900*                            IMS FUNKTIONSKODER                           
041000*01    -COPY W0003                                                        
041100     EJECT                                                                
041200* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA                      
041300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
041400 01  DLI-IO-AREA.                                                         
041500     03  IO-AREA-1               PIC X(71)  VALUE SPACE.                  
041600     SKIP3                                                                
041900*    03  WLKATH01 -COPY WDN501        -RED IO-AREA-1.                     
042000     EJECT                                                                
042100*    03  WLKATH11 -COPY WDN511        -RED IO-AREA-1.                     
042200     EJECT                                                                
042300*    03  WLKATH12 -COPY WDN512        -RED IO-AREA-1.                     
042400     EJECT                                                                
042410*    03  WLKATH13 -COPY WDN513        -RED IO-AREA-1.                     
042420     EJECT                                                                
042430*    03  WLKATH22 -COPY WDN522        -RED IO-AREA-1.                     
042440     EJECT                                                                
042500*    03  WLKATH24 -COPY WDN524        -RED IO-AREA-1.                     
042600     EJECT                                                                
042700*    03  WLKATH25 -COPY WDN525        -RED IO-AREA-1.                     
042800     EJECT                                                                
042900*    03  WLKATH26 -COPY WDN526        -RED IO-AREA-1.                     
043000     EJECT                                                                
043100     03  AVSG-IO-AREA            PIC X(48)  VALUE SPACE.                  
043200*    03  WLKATS01  -COPY WDN5G1       -RED AVSG-IO-AREA.                  
043201*        speglar WLKATH27 och WLKATH28 (SEQ)                              
043202                                                                          
043203*    --- Till denna area flyttas AVSG-IDWDN512 från AVSG-IO-AREA          
043211     03  AVSG-REF-IDWDN512.                                               
043220        09  AVSG-REF-IDCATNR       PIC 9(5).                              
043230        09  AVSG-REF-IDCATGRP      PIC 9(2).                              
043240        09  AVSG-REF-IDCATAVS      PIC 9(4).                              
043250        09  AVSG-REF-IDCATRAD      PIC 9(4).                              
043260        09  AVSG-REF-KDCATPUB-FOM  PIC X(6).                              
043270     EJECT                                                                
043300     03  IO-AREA-2               PIC X(500)  VALUE SPACE.                 
043400     SKIP3                                                                
043500*    03  WLKATM01 -COPY WDN101        -RED IO-AREA-2.                     
043600     EJECT                                                                
043700*    03  WLKATM11 -COPY WDN111        -RED IO-AREA-2.                     
043800     EJECT                                                                
043900*    03  WLKATB01 -COPY WDN201      -PRE RUB-  -RED IO-AREA-2.            
044000     EJECT                                                                
044100*    03  WLKATF01 -COPY WDN301      -PRE FOT-  -RED IO-AREA-2.            
044200     EJECT                                                                
044300*    03  WLKATL01 -COPY WDN7A1      -PRE ILLU- -RED IO-AREA-2.            
044400     EJECT                                                                
044500 LINKAGE SECTION.                                                         
044600     SKIP2                                                                
044700*01  -COPY W0009          -PRE MSG-                                       
044800     EJECT                                                                
044900*01  -COPY W0008          -PRE AVS-                                       
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008          -PRE RUB-                                       
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008          -PRE FOT-                                       
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008          -PRE ILLU-                                      
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100*01  -COPY W0008          -PRE KAT-                                       
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046400*01  -COPY W0008          -PRE AVS2-                                      
046500     05  FILLER                  PIC X.                                   
046600     EJECT                                                                
046700*01  -COPY W0008          -PRE AVS3-                                      
046800     05  FILLER                  PIC X.                                   
046900     EJECT                                                                
046910*01  -COPY W0008          -PRE KATS-                                      
046920     05  FILLER                  PIC X.                                   
046930     EJECT                                                                
047000 PROCEDURE DIVISION USING MSG-PCB AVS-PCB RUB-PCB                         
047100     FOT-PCB ILLU-PCB KAT-PCB AVS2-PCB AVS3-PCB KATS-PCB.                 
047200     ENTRY 'DLITCBL' USING MSG-PCB AVS-PCB RUB-PCB                        
047300     FOT-PCB ILLU-PCB KAT-PCB AVS2-PCB AVS3-PCB KATS-PCB.                 
047400     SKIP2                                                                
047500     PERFORM IMS-GET-MSG                                                  
047600                                                                          
047700     IF SEGMENT-FINNS                                                     
047800       PERFORM A-INIT                                                     
047900       IF (IDCATNR-WS NOT NUMERIC)                                        
048000       OR (IDCATGRP-WS NOT NUMERIC)                                       
048100       OR (IDCATAVS-WS NOT NUMERIC)                                       
048200         MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                           
048300         PERFORM G-RENSA-BILD                                             
048400         PERFORM X-GRUND-FORMAT                                           
048500       ELSE                                                               
048600         IF NYCKLAR-FEL = JA                                              
048700            MOVE FEL-3 (SPRAAK-IX) TO MOD-TEMFSFEL                        
048800            PERFORM G-RENSA-BILD                                          
048900            PERFORM X-GRUND-FORMAT                                        
049000         ELSE                                                             
049100           IF (KEY-IDCATGRP = ZERO)                                       
049200             MOVE FEL-7(SPRAAK-IX) TO MOD-TEMFSFEL                        
049300             PERFORM G-RENSA-BILD                                         
049400             PERFORM X-GRUND-FORMAT                                       
049500           ELSE                                                           
049600             EVALUATE TRUE                                                
049700               WHEN KEY-IDCATGRP < +20 AND > +09                          
049800                 MOVE FEL-8(SPRAAK-IX) TO MOD-TEMFSFEL                    
049900                 PERFORM G-RENSA-BILD                                     
050000                 PERFORM X-GRUND-FORMAT                                   
050100               WHEN OTHER                                                 
050200                 MOVE KEY-IDCATNR     TO W-IDCATNR                        
050300                 MOVE KEY-IDCATGRP    TO W-IDCATGRP                       
050400                 MOVE KEY-IDCATAVS    TO W-IDCATAVS                       
050500                 MOVE WS-KDCATPUB     TO W-KDCATPUB                       
050600                                                                          
050700                 IF MFS-UPDATE                                            
050800                   PERFORM B-KOLLA-INDATA                                 
050900                   IF INDATA-FEL = JA                                     
051000                     MOVE FEL-2 (SPRAAK-IX) TO MOD-TEMFSFEL               
051100                     PERFORM F-VISA-BILD-IGEN                             
051200                   ELSE                                                   
051300                     PERFORM IMS-GHU-AVS                                  
051400                     IF (W-IDCATGRP = 20 OR 30 OR 40 OR 50 OR 60          
051500                                         OR 70 OR 80 OR 90)               
051600                       AND W-IDCATAVS = ZERO                              
051700                       AND MID-IDILLU NOT = ALL '+'                       
051800                       PERFORM H-SKAPA-X0-ILLU                            
051900                       PERFORM G-RENSA-BILD                               
052000                       MOVE WS-KDCATPUB TO GALLANDE-KDCATPUB              
052100                       PERFORM E-LAS-KATALOGRAD                           
052200                       PERFORM X-GRUND-FORMAT                             
052300                     ELSE                                                 
052400                       IF W-IDCATAVS = ZERO                               
052500                         MOVE FEL-2 (SPRAAK-IX) TO MOD-TEMFSFEL           
052600                         PERFORM F-VISA-BILD-IGEN                         
052700                       ELSE                                               
052800                         PERFORM IMS-GHU-AVS                              
052900                         IF SEGMENT-SAKNAS                                
053000                           PERFORM C-ISRT-AVS                             
053100                           MOVE WS-KDCATPUB TO W-KDCATPUB                 
053200                         ELSE                                             
053300                           IF SW-BORTTAG = JA                             
053400                             PERFORM J-BORTTAG                            
053500                             PERFORM G-RENSA-BILD                         
053600                             PERFORM E-LAS-KATALOGRAD                     
053700                             PERFORM X-GRUND-FORMAT                       
053800                           ELSE                                           
053900                             IF SW-KOPIERING = JA                         
054000                               PERFORM K-KOPIERING                        
054100                               PERFORM G-RENSA-BILD                       
054200                               PERFORM E-LAS-KATALOGRAD                   
054300                               PERFORM X-GRUND-FORMAT                     
054400                             ELSE                                         
054500                               IF SW-KDCATPUB-FROM = JA                   
054600                                 PERFORM L-UPPDAT-KDCATPUB-FROM           
054700                                 PERFORM G-RENSA-BILD                     
054800                                 PERFORM E-LAS-KATALOGRAD                 
054900                                 PERFORM X-GRUND-FORMAT                   
055000                               END-IF                                     
055100                             END-IF                                       
055200                           END-IF                                         
055300                         END-IF                                           
055400                         IF INDATA-FEL = NEJ                              
055410                         AND SW-KOPIERING = NEJ                           
055500                         AND SW-BORTTAG = NEJ                             
055600                         AND SW-KDCATPUB-FROM = NEJ                       
055700                           PERFORM D-UPPDATERA                            
055800                           PERFORM G-RENSA-BILD                           
055900                           PERFORM E-LAS-KATALOGRAD                       
056000                           PERFORM X-GRUND-FORMAT                         
056100                         END-IF                                           
056200                       END-IF                                             
056300                     END-IF                                               
056400                   END-IF                                                 
056500                 ELSE                                                     
056600                   PERFORM Y-KOLLA-PF-TANGENTER                           
056700                   IF AVS-UTGANGSMARKERAT = JA                            
056800                     MOVE WS-KDCATPUB (4:3)                               
056810                                      TO MOD-KDCATPUB-R-UT                
056900                     MOVE FEL-19(SPRAAK-IX) TO MOD-TEMFSFEL               
057000                     PERFORM G-RENSA-BILD                                 
057100                   ELSE                                                   
057200                     PERFORM E-LAS-KATALOGRAD                             
057300                   END-IF                                                 
057400                   PERFORM X-GRUND-FORMAT                                 
057500                 END-IF                                                   
057600             END-EVALUATE                                                 
057700           END-IF                                                         
057800         END-IF                                                           
057900       END-IF                                                             
058000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
058100       PERFORM IMS-INSERT-MSG                                             
058200     END-IF                                                               
058300     MOVE ZERO TO RETURN-CODE                                             
058400     GOBACK                                                               
058500     .                                                                    
058600     EJECT                                                                
058700 A-INIT SECTION.                                                          
058800     SKIP2                                                                
058900     IF MSG-DUBBLA-TRANSKODER                                             
059000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I51401                 
059100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
059200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
059300       IF MFS-IDTRANS = '1514'                                            
059400         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
059500       ELSE                                                               
059600         MOVE SPACE TO MFS-KDTRTYP                                        
059700       END-IF                                                             
059800     ELSE                                                                 
059900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I51401                  
060000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
060100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
060200     END-IF                                                               
060300     MOVE MSG-IDPFK TO MFS-IDPFK                                          
060400                                                                          
060500*********                                                                 
060600     IF MFS-IDTRANS = '1514' or '1511' OR '1512' or '1513' or             
060700                   '1515'                                                 
060800        CONTINUE                                                          
060900     ELSE                                                                 
061000        MOVE ALL '+'         TO MID-IDCATNR-IN                            
061100                                MID-IDCATGRP-IN                           
061200                                MID-IDCATAVS-IN                           
061300                                MID-IDCATRAD-IN                           
061400                                MID-IDSKYLT-IN                            
061500                                MID-KDCATPUB-R-IN                         
061600        MOVE LOW-VALUE       to MID-KDCATPUB-R-NEXT                       
061700        MOVE SPACE           TO MID-IDCATNR-UT                            
061800                                MID-IDCATGRP-IN                           
061900                                MID-IDCATAVS-IN                           
062000                                MID-IDCATRAD-IN                           
062100                                MID-IDSKYLT-IN                            
062200                                MID-KDCATPUB-R-IN                         
062300     END-IF                                                               
062400*********                                                                 
062500                                                                          
062600     INSPECT MID-W1I51401 REPLACING ALL '>' BY SPACE                      
062700     INSPECT MID-W1I51401 REPLACING ALL '<' BY SPACE                      
062800                                                                          
062900     IF MID-IDCATNR-IN = ALL '+'                                          
063000       MOVE MID-IDCATNR-UT TO IDCATNR-WS                                  
063100       INSPECT IDCATNR-WS REPLACING LEADING SPACE BY ZERO                 
063200     ELSE                                                                 
063300       MOVE MID-IDCATNR-IN TO IDCATNR-WS                                  
063400     END-IF                                                               
063500     IF MID-IDCATGRP-IN = ALL '+'                                         
063600       MOVE MID-IDCATGRP-UT TO IDCATGRP-WS                                
063700       INSPECT IDCATGRP-WS REPLACING LEADING SPACE BY ZERO                
063800     ELSE                                                                 
063900       MOVE MID-IDCATGRP-IN TO IDCATGRP-WS                                
064000     END-IF                                                               
064100     IF MID-IDCATAVS-IN = ALL '+'                                         
064200       MOVE MID-IDCATAVS-UT TO IDCATAVS-WS                                
064300       INSPECT IDCATAVS-WS REPLACING LEADING SPACE BY ZERO                
064400     ELSE                                                                 
064500       MOVE MID-IDCATAVS-IN TO IDCATAVS-WS                                
064600     END-IF                                                               
064700     IF MID-IDCATRAD-IN = ALL '+'                                         
064800       MOVE MID-IDCATRAD-UT TO KEY-IDCATRAD-X                             
064900       INSPECT KEY-IDCATRAD-X REPLACING LEADING SPACE BY ZERO             
065000     ELSE                                                                 
065100       MOVE MID-IDCATRAD-IN TO KEY-IDCATRAD-X                             
065200     END-IF                                                               
065300                                                                          
065400     IF MID-IDCATNR-IN = ALL '+' AND                                      
065500        MID-IDCATAVS-IN = ALL '+' AND                                     
065600        MID-IDCATGRP-IN = ALL '+'                                         
065700          CONTINUE                                                        
065800     ELSE                                                                 
065900       MOVE SPACE TO MFS-KDTRTYP                                          
066000     END-IF                                                               
066100                                                                          
066200     IF MID-IDSKYLT-IN = ALL '+'                                          
066300       MOVE MID-IDSKYLT-UT TO W-IDSKYLT                                   
066400     ELSE                                                                 
066500       MOVE MID-IDSKYLT-IN TO W-IDSKYLT                                   
066600     END-IF                                                               
066700     INSPECT W-IDSKYLT CONVERTING GEMEN TO VERSAL                         
066800     IF W-IDSKYLT = SPACE                                                 
066900       MOVE 'S  '       TO W-IDSKYLT                                      
067000     END-IF                                                               
067100                                                                          
067200     MOVE LOW-VALUE TO MSG-AREA                                           
067300     MOVE 'W1O51401' TO MFS-IDMOD                                         
067400     MOVE '1514' TO MOD-IDTRANS                                           
067401                                                                          
067402*    -------  FIXA DATUMFÄLT                                              
067403                                                                          
067410     ACCEPT DAGENS-DATUM FROM DATE                                        
067420                                                                          
067430     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
067440     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
067450     CALL WDATKONV USING DAT-KDDATFORM                                    
067460                         DAT-I-TIDATUM                                    
067470                         DAT-O-TIDATUM                                    
067480                         DAT-KDSVAR                                       
067490     IF DAT-KDSVAR-OK                                                     
067491        MOVE DAT-TIAAVV-GRP TO DAGENS-AAR-VECKA                           
067492        MOVE DAGENS-V-VECKA TO DAGENS-VV                                  
067493        MOVE DAT-TISEKEL    TO DAGENS-AAAA(1:2)                           
067494     END-IF                                                               
067495                                                                          
067496     MOVE DAGENS-DATUM(1:2)  TO DAGENS-AAAA(3:2)                          
067497                                                                          
067498     COMPUTE WS-TIAAAA(1) = DAGENS-AAAA - 1                               
067499     COMPUTE WS-TIAAAA(2) = DAGENS-AAAA                                   
067500     COMPUTE WS-TIAAAA(3) = DAGENS-AAAA + 1                               
067501     COMPUTE WS-TIAAAA(4) = DAGENS-AAAA + 2                               
067510                                                                          
067520*    ---------  FLYTTA INPUT                                              
067530                                                                          
067600     IF KEY-IDCATRAD-X NUMERIC                                            
067700       IF KEY-IDCATRAD-X > ZERO                                           
067800         CONTINUE                                                         
067900       ELSE                                                               
068000         MOVE 20 TO KEY-IDCATRAD-NUM                                      
068100       END-IF                                                             
068200     ELSE                                                                 
068300       MOVE 20 TO KEY-IDCATRAD-NUM                                        
068400     END-IF                                                               
068500     IF ENGLISH-TEXT                                                      
068600       MOVE +2 TO SPRAAK-IX                                               
068700     ELSE                                                                 
068800       MOVE +1 TO SPRAAK-IX                                               
068900     END-IF                                                               
069000                                                                          
069100     MOVE IDCATNR-WS TO MOD-IDCATNR-UT                                    
069200     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
069300                                                                          
069400     MOVE IDCATGRP-WS TO MOD-IDCATGRP-UT                                  
069500     INSPECT MOD-IDCATGRP-UT REPLACING LEADING ZERO BY SPACE              
069600     IF MOD-IDCATGRP-UT = '   '                                           
069700       MOVE '  0' TO MOD-IDCATGRP-UT                                      
069800     END-IF                                                               
069900                                                                          
070000     MOVE IDCATAVS-WS TO MOD-IDCATAVS-UT                                  
070100     INSPECT MOD-IDCATAVS-UT REPLACING LEADING ZERO BY SPACE              
070200     IF MOD-IDCATAVS-UT = '     '                                         
070300       MOVE '    0' TO MOD-IDCATAVS-UT                                    
070400     END-IF                                                               
070500                                                                          
070600     MOVE W-IDSKYLT TO MOD-IDSKYLT-UT                                     
070700                                                                          
070800     MOVE KEY-IDCATRAD-X TO MOD-IDCATRAD-UT                               
070900     INSPECT MOD-IDCATRAD-UT REPLACING LEADING ZERO BY SPACE              
071000                                                                          
071100     MOVE NEJ TO NYCKLAR-FEL                                              
071200     IF MID-KDCATPUB-R-IN = ALL '+'                                       
071310        MOVE MID-KDCATPUB-R-UT TO WS-KDCATPUB-R-AVV                       
071320        PERFORM S50-Y2K-KDCATPUB-R                                        
071330        MOVE WS-KDCATPUB-AAAAVV  TO WS-KDCATPUB                           
071400     ELSE                                                                 
071510        MOVE MID-KDCATPUB-R-IN TO WS-KDCATPUB-R-AVV                       
071520        PERFORM S50-Y2K-KDCATPUB-R                                        
071530        MOVE WS-KDCATPUB-AAAAVV TO WS-KDCATPUB                            
071600        MOVE SPACE TO MFS-KDTRTYP                                         
071700     END-IF                                                               
071800     MOVE WS-KDCATPUB (4:3) TO MOD-KDCATPUB-R-UT                          
071900                                                                          
072000     IF WS-KDCATPUB = SPACE                                               
072110        MOVE LOW-VALUE TO WS-KDCATPUB                                     
072200     ELSE                                                                 
072300        IF WS-KDCATPUB NUMERIC                                            
072400           CONTINUE                                                       
072500        ELSE                                                              
072600           MOVE JA TO NYCKLAR-FEL                                         
072700        END-IF                                                            
072800     END-IF                                                               
072900                                                                          
073000     IF MID-KDCATPUB-R-NEXT NUMERIC                                       
073100        MOVE MID-KDCATPUB-R-NEXT TO WS-KDCATPUB-R-AVV                     
073120        PERFORM S50-Y2K-KDCATPUB-R                                        
073130        MOVE WS-KDCATPUB-AAAAVV  TO WS-KDCATPUB-NEXT                      
073200     ELSE                                                                 
073300        MOVE LOW-VALUE TO WS-KDCATPUB-NEXT                                
073400     END-IF                                                               
073500                                                                          
073600     IF WS-KDCATPUB-NEXT = SPACE                                          
073611*      --- Nästa PUBkod ligger utanför de godkända årtalen                
073612*      --- Beror sannolikt på att årsrensningen inte gått.                
073613*      --- Årsrensningen görs i W1543300 i W154Y1                         
073614*      --- Fixar här en PUBKOD för att komma förbi.                       
073615       IF MID-KDCATPUB-R-NEXT NUMERIC                                     
073616         IF MID-KDCATPUB-R-NEXT(1:1) > 5                                  
073617           STRING '199' MID-KDCATPUB-R-NEXT                               
073618                  DELIMITED BY SIZE                                       
073619                  INTO WS-KDCATPUB-NEXT                                   
073620         ELSE                                                             
073621           STRING '200' MID-KDCATPUB-R-NEXT                               
073622                  DELIMITED BY SIZE                                       
073623                  INTO WS-KDCATPUB-NEXT                                   
073630         END-IF                                                           
073640       END-IF                                                             
073800     END-IF                                                               
074980                                                                          
075000     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
075100                             MOD-IDCATGRP-IN                              
075200                             MOD-IDCATAVS-IN                              
075300                             MOD-IDCATRAD-IN                              
075400                             MOD-IDSKYLT-IN                               
075500                             MOD-KDCATPUB-R-IN                            
075600                             MOD-KDCATPUB-R-NEXT                          
075700                             MOD-TEMFSFEL                                 
075800                             MOD-TEMFSINF                                 
075900     .                                                                    
076000     EJECT                                                                
076100 B-KOLLA-INDATA SECTION.                                                  
076200     SKIP2                                                                
076300     MOVE NEJ TO INDATA-FEL                                               
076400     MOVE NEJ TO IDILLU-FINNS                                             
076500     MOVE NEJ TO IDRUBNR-FINNS                                            
076600     MOVE NEJ TO IDFOTNR-RAD4-FINNS                                       
076700     MOVE NEJ TO IDFOTNR-FINNS                                            
076800     MOVE NEJ TO TEKOL-FINNS                                              
076900     MOVE NEJ TO TEKOL-FINNS                                              
077000     MOVE 'J' TO IN-FLRUBTYP                                              
077100                                                                          
077200     IF MID-BORT = ALL '+' OR SPACE                                       
077300        MOVE SPACE TO WS-BORT                                             
077400     ELSE                                                                 
077500        IF MID-BORT = 'J' OR 'Y' or 'j' or 'y'                            
077600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BORT-ATTR                     
077700           MOVE MID-BORT TO WS-BORT                                       
077800        ELSE                                                              
077900           MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR                       
078000           MOVE JA TO INDATA-FEL                                          
078100        END-IF                                                            
078200     END-IF                                                               
078300                                                                          
078400     IF MID-KDCATPUB-R-COPY = ALL '+'                                     
078500        CONTINUE                                                          
078600     ELSE                                                                 
078700        IF MID-KDCATPUB-R-COPY NUMERIC                                    
078800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-COPY-ATTR          
078900        ELSE                                                              
079000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR            
079100           MOVE JA TO INDATA-FEL                                          
079200        END-IF                                                            
079300     END-IF                                                               
079400                                                                          
079500     IF MID-KDCATPUB-R-FROM = ALL '+'                                     
079600        CONTINUE                                                          
079700     ELSE                                                                 
079800        IF MID-KDCATPUB-R-FROM NUMERIC                                    
079900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-FROM-ATTR          
080000        ELSE                                                              
080100           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-FROM-ATTR            
080200           MOVE JA TO INDATA-FEL                                          
080300        END-IF                                                            
080400     END-IF                                                               
080500                                                                          
080600     IF MID-KDCATPUB-R-TOM = ALL '+'                                      
080700        CONTINUE                                                          
080800     ELSE                                                                 
080900        IF MID-KDCATPUB-R-TOM NUMERIC                                     
081000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-TOM-ATTR           
081100        ELSE                                                              
081200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR             
081300           MOVE JA TO INDATA-FEL                                          
081400        END-IF                                                            
081500     END-IF                                                               
081600                                                                          
081700     PERFORM BJ-KOLLA-BEHANDLING-KDCATPUB                                 
081800                                                                          
081900     IF SW-KOPIERING = JA                                                 
082000        OR SW-BORTTAG = JA OR SW-KDCATPUB-FROM = JA                       
082100                                                                          
082200        PERFORM BI-KOLLA-MID-EJ-IFYLLD                                    
082300                                                                          
082400        IF SW-BORTTAG = JA OR SW-KDCATPUB-FROM = JA                       
082500           IF MID-KDCATPUB-R-TOM NOT = ALL '+'                            
082600              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR          
082700              MOVE JA TO INDATA-FEL                                       
082800           END-IF                                                         
082900        END-IF                                                            
083000     ELSE                                                                 
083100                                                                          
083200        PERFORM BH-NOLLSTALL-IN                                           
083300                                                                          
083400        MOVE +1 TO INDX                                                   
083500        PERFORM UNTIL INDX >= +6                                          
083600          IF MID-IDRUBNR(INDX) NOT = ALL '+'                              
083700            IF MID-IDRUBNR(INDX) NUMERIC                                  
083800              MOVE JA TO IDRUBNR-FINNS                                    
083900              MOVE MID-IDRUBNR(INDX) TO IN-IDRUBNR(INDX)                  
084000              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRUBNR-ATTR                
084100                                   (INDX)                                 
084200            ELSE                                                          
084300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(INDX)            
084400              MOVE JA TO INDATA-FEL                                       
084500            END-IF                                                        
084600          END-IF                                                          
084700          ADD +1 TO INDX                                                  
084800        END-PERFORM                                                       
084900                                                                          
085000        MOVE +1 TO INDX                                                   
085100        PERFORM UNTIL  INDX >= +6                                         
085200          IF MID-TEKOL(INDX) NOT = ALL '+'                                
085300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEKOL-ATTR(INDX)             
085400            MOVE JA TO TEKOL-FINNS                                        
085500          END-IF                                                          
085600          ADD +1 TO INDX                                                  
085700        END-PERFORM                                                       
085800                                                                          
085900        IF MID-BERUBTEXT-1 NOT = ALL '+'                                  
086000          MOVE MFS-ALFA-FAELT-RAETT TO MOD-BERUBTEXT-1-ATTR               
086100          MOVE JA TO BERUBTEXT-FINNS                                      
086200        END-IF                                                            
086300        IF MID-BERUBTEXT-2 NOT = ALL '+'                                  
086400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-BERUBTEXT-2-ATTR               
086500          MOVE JA TO BERUBTEXT-FINNS                                      
086600        END-IF                                                            
086700                                                                          
086800        MOVE +1 TO INDX                                                   
086900        PERFORM UNTIL INDX >= +4                                          
087000          IF MID-IDFOTNR-RAD4(INDX) NOT = ALL '+'                         
087100            IF MID-IDFOTNR-RAD4(INDX) NUMERIC                             
087200              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFOTNR-RAD4-ATTR           
087300                                          (INDX)                          
087400              MOVE JA TO IDFOTNR-RAD4-FINNS                               
087500              MOVE MID-IDFOTNR-RAD4(INDX) TO IN-IDFOTNR-RAD4              
087600                                          (INDX)                          
087700            ELSE                                                          
087800              MOVE MFS-NUM-FAELT-FEL TO MOD-IDFOTNR-RAD4-ATTR             
087900                                          (INDX)                          
088000              MOVE JA TO INDATA-FEL                                       
088100            END-IF                                                        
088200          END-IF                                                          
088300          ADD +1 TO INDX                                                  
088400        END-PERFORM                                                       
088500                                                                          
088600        MOVE +1 TO GRP-IX                                                 
088700        PERFORM UNTIL  GRP-IX >= +6                                       
088800          MOVE +1 TO INDX                                                 
088900          PERFORM UNTIL INDX >= +4                                        
089000            IF MID-IDFOTNR(GRP-IX, INDX) NOT = ALL '+'                    
089100              IF MID-IDFOTNR(GRP-IX, INDX) NUMERIC                        
089200                MOVE MFS-NUM-FAELT-RAETT TO                               
089300                          MOD-IDFOTNR-ATTR(GRP-IX, INDX)                  
089400                MOVE JA TO IDFOTNR-FINNS                                  
089500              ELSE                                                        
089600                MOVE MFS-NUM-FAELT-FEL TO                                 
089700                          MOD-IDFOTNR-ATTR(GRP-IX, INDX)                  
089800                MOVE JA TO INDATA-FEL                                     
089900              END-IF                                                      
090000            END-IF                                                        
090100            ADD +1 TO INDX                                                
090200          END-PERFORM                                                     
090300          ADD +1 TO GRP-IX                                                
090400        END-PERFORM                                                       
090500                                                                          
090600        IF MID-IDILLU NOT = ALL '+'                                       
090700          IF MID-IDILLU NUMERIC                                           
090800*           IF MID-IDILLU = ZERO                                          
090900*              MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-ATTR                  
091000*              MOVE JA TO INDATA-FEL                                      
091100*           ELSE                                                          
091200               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-ATTR                
091300               MOVE JA TO IDILLU-FINNS                                    
091400*           END-IF                                                        
091500          ELSE                                                            
091600            MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-ATTR                     
091700            MOVE JA TO INDATA-FEL                                         
091800          END-IF                                                          
091900        END-IF                                                            
092000        IF INDATA-FEL = NEJ AND IDRUBNR-FINNS = JA                        
092100          IF (IN-IDRUBNR(2) NOT = ZERO) OR                                
092200          (IN-IDRUBNR(3) NOT = ZERO)                                      
092300            IF (IN-IDRUBNR(4) = ZERO)                                     
092400              CONTINUE                                                    
092500            ELSE                                                          
092600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(4)               
092700              MOVE JA TO INDATA-FEL                                       
092800            END-IF                                                        
092900            IF (IN-IDRUBNR(5) = ZERO)                                     
093000              CONTINUE                                                    
093100            ELSE                                                          
093200              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(5)               
093300              MOVE JA TO INDATA-FEL                                       
093400            END-IF                                                        
093500          END-IF                                                          
093600          IF INDATA-FEL = NEJ                                             
093700            IF (IN-IDRUBNR(4) NOT = ZERO) OR                              
093800            (IN-IDRUBNR(5) NOT = ZERO)                                    
093900                                                                          
094000              MOVE 'N' TO IN-FLRUBTYP                                     
094100            END-IF                                                        
094200          END-IF                                                          
094300        END-IF                                                            
094400        IF INDATA-FEL = NEJ                                               
094500          PERFORM IMS-GHU-AVS                                             
094600          IF SEGMENT-FINNS                                                
094700            MOVE JA TO ROT-FINNS                                          
094800          ELSE                                                            
094900            MOVE NEJ TO ROT-FINNS                                         
095000          END-IF                                                          
095100          IF IDILLU-FINNS = JA                                            
095200            PERFORM BA-LAS-ILLU                                           
095300          END-IF                                                          
095400          IF IDRUBNR-FINNS = JA                                           
095500            PERFORM BB-LAS-BAS-RUBNR                                      
095600          END-IF                                                          
095700          IF IDFOTNR-RAD4-FINNS = JA                                      
095800            PERFORM BC-LAS-BAS-FOTNR-RAD4                                 
095900          END-IF                                                          
096000          IF IDFOTNR-FINNS = JA                                           
096100            PERFORM BD-LAS-BAS-FOTNR                                      
096200          END-IF                                                          
096300          IF INDATA-FEL = NEJ                                             
096400            IF IDRUBNR-FINNS = JA                                         
096500              PERFORM BE-KOLLA-RUB-MED-BAS                                
096600            END-IF                                                        
096700            IF IDFOTNR-RAD4-FINNS = JA                                    
096800            OR IDFOTNR-FINNS = JA                                         
096900              PERFORM BF-KOLLA-FOT-MED-BAS                                
097000            END-IF                                                        
097100            IF INDATA-FEL = NEJ AND IDRUBNR-FINNS = JA                    
097200              PERFORM BG-FLYTTA-TEST-INBAS                                
097300            END-IF                                                        
097400          END-IF                                                          
097500        END-IF                                                            
097600     END-IF                                                               
097700     .                                                                    
097800     EJECT                                                                
097900 BA-LAS-ILLU SECTION.                                                     
098000     SKIP2                                                                
098100     IF MID-IDILLU = ZERO                                                 
098200       CONTINUE                                                           
098300     ELSE                                                                 
098400       MOVE MID-IDILLU TO W-IDILLU                                        
098500       PERFORM IMS-GU-ILLU                                                
098600       IF SEGMENT-SAKNAS                                                  
098700         MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-ATTR                        
098800         MOVE JA TO INDATA-FEL                                            
098900       END-IF                                                             
099000     END-IF                                                               
099100     .                                                                    
099200     EJECT                                                                
099300 BB-LAS-BAS-RUBNR SECTION.                                                
099400     SKIP2                                                                
099500     MOVE ZERO TO BAS-IDRUBNR(1)                                          
099600                  BAS-IDRUBNR(2)                                          
099700                  BAS-IDRUBNR(3)                                          
099800     MOVE IN-FLRUBTYP TO BAS-FLRUBTYP                                     
099900                                                                          
100000     MOVE ZERO TO TEST-IDRUBNR(1)                                         
100100                  TEST-IDRUBNR(2)                                         
100200                  TEST-IDRUBNR(3)                                         
100300                  TEST-IDRUBNR(4)                                         
100400                  TEST-IDRUBNR(5)                                         
100500                                                                          
100600     IF ROT-FINNS = JA                                                    
100700       MOVE +1 TO W-IDCATRAD                                              
100800       MOVE +1 TO INDX                                                    
100900       PERFORM IMS-GHNP-AVS-RAD                                           
101000       IF SEGMENT-FINNS                                                   
101100         PERFORM IMS-GHNP-AVS-RUB                                         
101200         PERFORM UNTIL INDX >= +4 OR SEGMENT-SAKNAS                       
101300           MOVE RUB-IDRUBNR TO BAS-IDRUBNR(RUB-IDSEGMNR)                  
101400           MOVE RUB-FLRUBTYP TO BAS-FLRUBTYP                              
101500           ADD +1 TO INDX                                                 
101600           PERFORM IMS-GHNP-AVS-RUB                                       
101700         END-PERFORM                                                      
101800       END-IF                                                             
101900     END-IF                                                               
102000     MOVE BAS-IDRUBNR(1) TO TEST-IDRUBNR(1)                               
102100     IF BAS-FLRUBTYP = 'J'                                                
102200       MOVE BAS-IDRUBNR(2) TO TEST-IDRUBNR(2)                             
102300       MOVE BAS-IDRUBNR(3) TO TEST-IDRUBNR(3)                             
102400     ELSE                                                                 
102500       MOVE BAS-IDRUBNR(2) TO TEST-IDRUBNR(4)                             
102600       MOVE BAS-IDRUBNR(3) TO TEST-IDRUBNR(5)                             
102700     END-IF                                                               
102800     IF MID-IDRUBNR(1) NOT = ALL '+'                                      
102900       MOVE MID-IDRUBNR(1) TO TEST-IDRUBNR(1)                             
103000     END-IF                                                               
103100     IF MID-IDRUBNR(2) NOT = ALL '+'                                      
103200       MOVE MID-IDRUBNR(2) TO TEST-IDRUBNR(2)                             
103300     END-IF                                                               
103400     IF MID-IDRUBNR(3) NOT = ALL '+'                                      
103500       MOVE MID-IDRUBNR(3) TO TEST-IDRUBNR(3)                             
103600     END-IF                                                               
103700     IF MID-IDRUBNR(4) NOT = ALL '+'                                      
103800       MOVE MID-IDRUBNR(4) TO TEST-IDRUBNR(4)                             
103900     END-IF                                                               
104000     IF MID-IDRUBNR(5) NOT = ALL '+'                                      
104100       MOVE MID-IDRUBNR(5) TO TEST-IDRUBNR(5)                             
104200     END-IF                                                               
104300     IF TEST-IDRUBNR(2) NOT = ZERO OR                                     
104400     TEST-IDRUBNR(3) NOT = ZERO                                           
104500       IF TEST-IDRUBNR(4) NOT = ZERO OR                                   
104600       TEST-IDRUBNR(5) NOT = ZERO                                         
104700         IF MID-IDRUBNR(2) NUMERIC AND MID-IDRUBNR(2) > ZERO              
104800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(2)                  
104900           MOVE JA TO INDATA-FEL                                          
105000         END-IF                                                           
105100         IF MID-IDRUBNR(3) NUMERIC AND MID-IDRUBNR(3) > ZERO              
105200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(3)                  
105300           MOVE JA TO INDATA-FEL                                          
105400         END-IF                                                           
105500         IF MID-IDRUBNR(4) NUMERIC AND MID-IDRUBNR(4) > ZERO              
105600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(4)                  
105700           MOVE JA TO INDATA-FEL                                          
105800         END-IF                                                           
105900         IF MID-IDRUBNR(5) NUMERIC AND MID-IDRUBNR(5) > ZERO              
106000           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(5)                  
106100           MOVE JA TO INDATA-FEL                                          
106200         END-IF                                                           
106300       END-IF                                                             
106400     END-IF                                                               
106500     IF INDATA-FEL = NEJ                                                  
106600       IF TEST-IDRUBNR(2) NOT = ZERO OR                                   
106700       TEST-IDRUBNR(3) NOT = ZERO                                         
106800         MOVE 'J' TO TEST-FLRUBTYP                                        
106900       ELSE                                                               
107000         IF TEST-IDRUBNR(4) NOT = ZERO OR                                 
107100         TEST-IDRUBNR(5) NOT = ZERO                                       
107200           MOVE 'N' TO TEST-FLRUBTYP                                      
107300         ELSE                                                             
107400           IF TEST-IDRUBNR(1) NOT = ZERO                                  
107500             MOVE 'J' TO TEST-FLRUBTYP                                    
107600           END-IF                                                         
107700         END-IF                                                           
107800       END-IF                                                             
107900     END-IF                                                               
108000     IF INDATA-FEL = NEJ                                                  
108100       IF TEST-FLRUBTYP = 'J'                                             
108200         IF TEST-IDRUBNR(1) NOT = ZERO                                    
108300           IF TEST-IDRUBNR(1) = TEST-IDRUBNR(2)                           
108400             IF MID-IDRUBNR(2) NOT = ALL '+'                              
108500               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(2)              
108600               MOVE JA TO INDATA-FEL                                      
108700             ELSE                                                         
108800               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(1)              
108900               MOVE JA TO INDATA-FEL                                      
109000             END-IF                                                       
109100           END-IF                                                         
109200           IF TEST-IDRUBNR(1) = TEST-IDRUBNR(3)                           
109300             IF MID-IDRUBNR(3) NOT = ALL '+'                              
109400               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(3)              
109500               MOVE JA TO INDATA-FEL                                      
109600             ELSE                                                         
109700               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(1)              
109800               MOVE JA TO INDATA-FEL                                      
109900             END-IF                                                       
110000           END-IF                                                         
110100           IF TEST-IDRUBNR(2) NOT = ZERO                                  
110200             IF TEST-IDRUBNR(2) = TEST-IDRUBNR(3)                         
110300               IF MID-IDRUBNR(3) NOT = ALL '+'                            
110400                 MOVE MFS-NUM-FAELT-FEL TO                                
110500                      MOD-IDRUBNR-ATTR(3)                                 
110600                 MOVE JA TO INDATA-FEL                                    
110700               ELSE                                                       
110800                 MOVE MFS-NUM-FAELT-FEL TO                                
110900                      MOD-IDRUBNR-ATTR(2)                                 
111000                 MOVE JA TO INDATA-FEL                                    
111100               END-IF                                                     
111200             END-IF                                                       
111300           END-IF                                                         
111400         ELSE                                                             
111500           IF TEST-IDRUBNR(2) NOT = ZERO                                  
111600             IF TEST-IDRUBNR(2) = TEST-IDRUBNR(3)                         
111700               IF MID-IDRUBNR(3) NOT = ALL '+'                            
111800                 MOVE MFS-NUM-FAELT-FEL TO                                
111900                      MOD-IDRUBNR-ATTR(3)                                 
112000                 MOVE JA TO INDATA-FEL                                    
112100               ELSE                                                       
112200                 MOVE MFS-NUM-FAELT-FEL TO                                
112300                      MOD-IDRUBNR-ATTR(2)                                 
112400                 MOVE JA TO INDATA-FEL                                    
112500               END-IF                                                     
112600             END-IF                                                       
112700           END-IF                                                         
112800         END-IF                                                           
112900       ELSE                                                               
113000         IF TEST-IDRUBNR(1) NOT = ZERO                                    
113100           IF TEST-IDRUBNR(1) = TEST-IDRUBNR(4)                           
113200             IF MID-IDRUBNR(4) NOT = ALL '+'                              
113300               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(4)              
113400               MOVE JA TO INDATA-FEL                                      
113500             ELSE                                                         
113600               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(1)              
113700               MOVE JA TO INDATA-FEL                                      
113800             END-IF                                                       
113900           END-IF                                                         
114000           IF TEST-IDRUBNR(1) = TEST-IDRUBNR(5)                           
114100             IF MID-IDRUBNR(5) NOT = ALL '+'                              
114200               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(5)              
114300               MOVE JA TO INDATA-FEL                                      
114400             ELSE                                                         
114500               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(1)              
114600               MOVE JA TO INDATA-FEL                                      
114700             END-IF                                                       
114800           END-IF                                                         
114900           IF TEST-IDRUBNR(4) NOT = ZERO                                  
115000             IF TEST-IDRUBNR(4) = TEST-IDRUBNR(5)                         
115100               IF MID-IDRUBNR(5) NOT = ALL '+'                            
115200                 MOVE MFS-NUM-FAELT-FEL TO                                
115300                      MOD-IDRUBNR-ATTR(5)                                 
115400                 MOVE JA TO INDATA-FEL                                    
115500               ELSE                                                       
115600                 MOVE MFS-NUM-FAELT-FEL TO                                
115700                      MOD-IDRUBNR-ATTR(4)                                 
115800                 MOVE JA TO INDATA-FEL                                    
115900               END-IF                                                     
116000             END-IF                                                       
116100           END-IF                                                         
116200         ELSE                                                             
116300           IF TEST-IDRUBNR(4) NOT = ZERO                                  
116400             IF TEST-IDRUBNR(4) = TEST-IDRUBNR(5)                         
116500               IF MID-IDRUBNR(5) NOT = ALL '+'                            
116600                 MOVE MFS-NUM-FAELT-FEL TO                                
116700                      MOD-IDRUBNR-ATTR(5)                                 
116800                 MOVE JA TO INDATA-FEL                                    
116900               ELSE                                                       
117000                 MOVE MFS-NUM-FAELT-FEL TO                                
117100                      MOD-IDRUBNR-ATTR(4)                                 
117200                 MOVE JA TO INDATA-FEL                                    
117300               END-IF                                                     
117400             END-IF                                                       
117500           END-IF                                                         
117600         END-IF                                                           
117700       END-IF                                                             
117800     END-IF                                                               
117900     .                                                                    
118000     EJECT                                                                
118100 BC-LAS-BAS-FOTNR-RAD4 SECTION.                                           
118200     SKIP2                                                                
118300     MOVE ZERO TO BAS-IDFOTNR-RAD4(1)                                     
118400                  BAS-IDFOTNR-RAD4(2)                                     
118500                  BAS-IDFOTNR-RAD4(3)                                     
118600                                                                          
118700     MOVE ZERO TO TEST-IDFOTNR-RAD4(1)                                    
118800                  TEST-IDFOTNR-RAD4(2)                                    
118900                  TEST-IDFOTNR-RAD4(3)                                    
119000                                                                          
119100     IF ROT-FINNS = JA                                                    
119200       MOVE +4 TO W-IDCATRAD                                              
119300       MOVE +1 TO INDX                                                    
119400       PERFORM IMS-GHNP-AVS-RAD                                           
119500       IF SEGMENT-FINNS                                                   
119600         PERFORM IMS-GHNP-AVS-FOT                                         
119700         PERFORM UNTIL SEGMENT-SAKNAS OR INDX >= +4                       
119800           MOVE FOT-IDFOTNR TO                                            
119900                BAS-IDFOTNR-RAD4(FOT-IDSEGMNR)                            
120000           ADD +1 TO INDX                                                 
120100           PERFORM IMS-GHNP-AVS-FOT                                       
120200         END-PERFORM                                                      
120300       END-IF                                                             
120400     END-IF                                                               
120500     PERFORM BCA-FLYTTA-TILL-TEST                                         
120600                                                                          
120700*- - - -  - - - - - - - - - - LIKA IDFOTNR                                
120800     IF TEST-IDFOTNR-RAD4(1) NOT = ZERO                                   
120900       IF TEST-IDFOTNR-RAD4(1) = TEST-IDFOTNR-RAD4(2)                     
121000         IF MID-IDFOTNR-RAD4(2) NOT = ALL '+'                             
121100           MOVE MFS-NUM-FAELT-FEL TO                                      
121200                MOD-IDFOTNR-RAD4-ATTR(2)                                  
121300           MOVE JA TO INDATA-FEL                                          
121400         ELSE                                                             
121500           MOVE MFS-NUM-FAELT-FEL TO                                      
121600                MOD-IDFOTNR-RAD4-ATTR(1)                                  
121700           MOVE JA TO INDATA-FEL                                          
121800         END-IF                                                           
121900       END-IF                                                             
122000       IF TEST-IDFOTNR-RAD4(1) = TEST-IDFOTNR-RAD4(3)                     
122100         IF MID-IDFOTNR-RAD4(3) NOT = ALL '+'                             
122200           MOVE MFS-NUM-FAELT-FEL TO                                      
122300                MOD-IDFOTNR-RAD4-ATTR(3)                                  
122400           MOVE JA TO INDATA-FEL                                          
122500         ELSE                                                             
122600           MOVE MFS-NUM-FAELT-FEL TO                                      
122700                MOD-IDFOTNR-RAD4-ATTR(1)                                  
122800           MOVE JA TO INDATA-FEL                                          
122900         END-IF                                                           
123000       END-IF                                                             
123100       IF TEST-IDFOTNR-RAD4(2) NOT = ZERO                                 
123200         IF TEST-IDFOTNR-RAD4(2) = TEST-IDFOTNR-RAD4(3)                   
123300           IF MID-IDFOTNR-RAD4(3) NOT = ALL '+'                           
123400             MOVE MFS-NUM-FAELT-FEL TO                                    
123500                  MOD-IDFOTNR-RAD4-ATTR(3)                                
123600             MOVE JA TO INDATA-FEL                                        
123700           ELSE                                                           
123800             MOVE MFS-NUM-FAELT-FEL TO                                    
123900                  MOD-IDFOTNR-RAD4-ATTR(1)                                
124000             MOVE JA TO INDATA-FEL                                        
124100           END-IF                                                         
124200         END-IF                                                           
124300       END-IF                                                             
124400     ELSE                                                                 
124500       IF TEST-IDFOTNR-RAD4(2) NOT = ZERO                                 
124600         IF TEST-IDFOTNR-RAD4(2) = TEST-IDFOTNR-RAD4(3)                   
124700           IF MID-IDFOTNR-RAD4(3) NOT = ALL '+'                           
124800             MOVE MFS-NUM-FAELT-FEL TO                                    
124900                  MOD-IDFOTNR-RAD4-ATTR(3)                                
125000             MOVE JA TO INDATA-FEL                                        
125100           ELSE                                                           
125200             MOVE MFS-NUM-FAELT-FEL TO                                    
125300                  MOD-IDFOTNR-RAD4-ATTR(2)                                
125400             MOVE JA TO INDATA-FEL                                        
125500           END-IF                                                         
125600         END-IF                                                           
125700       END-IF                                                             
125800     END-IF                                                               
125900     .                                                                    
126000     EJECT                                                                
126100 BCA-FLYTTA-TILL-TEST SECTION.                                            
126200     SKIP2                                                                
126300     MOVE BAS-IDFOTNR-RAD4(1) TO TEST-IDFOTNR-RAD4(1)                     
126400     MOVE BAS-IDFOTNR-RAD4(2) TO TEST-IDFOTNR-RAD4(2)                     
126500     MOVE BAS-IDFOTNR-RAD4(3) TO TEST-IDFOTNR-RAD4(3)                     
126600                                                                          
126700     IF MID-IDFOTNR-RAD4(1) NOT = ALL '+'                                 
126800       MOVE MID-IDFOTNR-RAD4(1) TO TEST-IDFOTNR-RAD4(1)                   
126900     END-IF                                                               
127000     IF MID-IDFOTNR-RAD4(2) NOT = ALL '+'                                 
127100       MOVE MID-IDFOTNR-RAD4(2) TO TEST-IDFOTNR-RAD4(2)                   
127200     END-IF                                                               
127300     IF MID-IDFOTNR-RAD4(3) NOT = ALL '+'                                 
127400       MOVE MID-IDFOTNR-RAD4(3) TO TEST-IDFOTNR-RAD4(3)                   
127500     END-IF                                                               
127600     .                                                                    
127700     EJECT                                                                
127800 BD-LAS-BAS-FOTNR SECTION.                                                
127900     SKIP2                                                                
128000     PERFORM BDA-NOLLSTALL-FOTNR                                          
128100                                                                          
128200     IF ROT-FINNS = JA                                                    
128300       MOVE +1 TO GRP-IX                                                  
128400       PERFORM UNTIL GRP-IX >= +6                                         
128500         PERFORM BDB-LAS-FOTNR                                            
128600         ADD +1 TO GRP-IX                                                 
128700       END-PERFORM                                                        
128800     END-IF                                                               
128900     PERFORM BDC-FLYTTA-BAS-TILL-TEST                                     
129000     PERFORM BDD-FLYTTA-MID-TILL-TEST                                     
129100                                                                          
129200     MOVE +1 TO GRP-IX                                                    
129300     PERFORM UNTIL GRP-IX >= +6                                           
129400       PERFORM BDE-KOLLA-LIKA-IDFOTNR                                     
129500       ADD +1 TO GRP-IX                                                   
129600     END-PERFORM                                                          
129700     .                                                                    
129800     EJECT                                                                
129900 BDA-NOLLSTALL-FOTNR SECTION.                                             
130000     SKIP2                                                                
130100     MOVE +1 TO GRP-IX                                                    
130200     PERFORM UNTIL GRP-IX >= +6                                           
130300       MOVE +1 TO INDX                                                    
130400       PERFORM UNTIL INDX >= +4                                           
130500         MOVE ZERO TO BAS-IDFOTNR(GRP-IX, INDX)                           
130600         ADD +1 TO INDX                                                   
130700       END-PERFORM                                                        
130800       ADD +1 TO GRP-IX                                                   
130900     END-PERFORM                                                          
131000     .                                                                    
131100     EJECT                                                                
131200 BDB-LAS-FOTNR SECTION.                                                   
131300     SKIP2                                                                
131400     EVALUATE GRP-IX                                                      
131500     WHEN 1                                                               
131600       MOVE 10 TO W-IDCATRAD                                              
131700     WHEN 2                                                               
131800       MOVE 11 TO W-IDCATRAD                                              
131900     WHEN 3                                                               
132000       MOVE 12 TO W-IDCATRAD                                              
132100     WHEN 4                                                               
132200       MOVE 13 TO W-IDCATRAD                                              
132300     WHEN 5                                                               
132400       MOVE 14 TO W-IDCATRAD                                              
132500     END-EVALUATE                                                         
132600     PERFORM IMS-GHNP-AVS-RAD                                             
132700     IF SEGMENT-FINNS                                                     
132800       MOVE +1 TO INDX                                                    
132900       PERFORM IMS-GHNP-AVS-FOT                                           
133000       PERFORM UNTIL SEGMENT-SAKNAS OR  INDX >= +4                        
133100         MOVE FOT-IDFOTNR TO                                              
133200              BAS-IDFOTNR(GRP-IX, FOT-IDSEGMNR)                           
133300         ADD +1 TO INDX                                                   
133400         PERFORM IMS-GHNP-AVS-FOT                                         
133500       END-PERFORM                                                        
133600     END-IF                                                               
133700     .                                                                    
133800     EJECT                                                                
133900 BDC-FLYTTA-BAS-TILL-TEST SECTION.                                        
134000     SKIP2                                                                
134100     MOVE +1 TO GRP-IX                                                    
134200     PERFORM UNTIL GRP-IX >= +6                                           
134300       MOVE +1 TO INDX                                                    
134400       PERFORM UNTIL INDX >= +4                                           
134500         MOVE BAS-IDFOTNR(GRP-IX, INDX) TO                                
134600              TEST-IDFOTNR(GRP-IX, INDX)                                  
134700         ADD +1 TO INDX                                                   
134800       END-PERFORM                                                        
134900       ADD +1 TO GRP-IX                                                   
135000     END-PERFORM                                                          
135100     .                                                                    
135200     EJECT                                                                
135300 BDD-FLYTTA-MID-TILL-TEST SECTION.                                        
135400     SKIP2                                                                
135500     MOVE +1 TO GRP-IX                                                    
135600     PERFORM UNTIL GRP-IX >= +6                                           
135700       MOVE +1 TO INDX                                                    
135800       PERFORM UNTIL INDX >= +4                                           
135900         IF MID-IDFOTNR(GRP-IX, INDX) NOT = ALL '+'                       
136000           MOVE MID-IDFOTNR(GRP-IX, INDX) TO                              
136100                                    TEST-IDFOTNR(GRP-IX, INDX)            
136200         END-IF                                                           
136300         ADD +1 TO INDX                                                   
136400       END-PERFORM                                                        
136500       ADD +1 TO GRP-IX                                                   
136600     END-PERFORM                                                          
136700     .                                                                    
136800     EJECT                                                                
136900 BDE-KOLLA-LIKA-IDFOTNR SECTION.                                          
137000     SKIP2                                                                
137100*- - - -  - - - - - - - - - - LIKA IDFOTNR                                
137200     IF INDATA-FEL = NEJ                                                  
137300       IF TEST-IDFOTNR(GRP-IX, 1) NOT = ZERO                              
137400                                                                          
137500         IF TEST-IDFOTNR(GRP-IX, 1) = TEST-IDFOTNR(GRP-IX, 2)             
137600           IF MID-IDFOTNR(GRP-IX, 2) NOT = ALL '+'                        
137700             MOVE MFS-NUM-FAELT-FEL TO                                    
137800                                    MOD-IDFOTNR-ATTR(GRP-IX, 2)           
137900             MOVE JA TO INDATA-FEL                                        
138000           ELSE                                                           
138100             MOVE MFS-NUM-FAELT-FEL TO                                    
138200                                    MOD-IDFOTNR-ATTR(GRP-IX, 1)           
138300             MOVE JA TO INDATA-FEL                                        
138400           END-IF                                                         
138500         END-IF                                                           
138600                                                                          
138700         IF TEST-IDFOTNR(GRP-IX, 1) = TEST-IDFOTNR(GRP-IX, 3)             
138800           IF MID-IDFOTNR(GRP-IX, 3) NOT = ALL '+'                        
138900             MOVE MFS-NUM-FAELT-FEL TO                                    
139000                                    MOD-IDFOTNR-ATTR(GRP-IX, 3)           
139100             MOVE JA TO INDATA-FEL                                        
139200           ELSE                                                           
139300             MOVE MFS-NUM-FAELT-FEL TO                                    
139400                                    MOD-IDFOTNR-ATTR(GRP-IX, 1)           
139500             MOVE JA TO INDATA-FEL                                        
139600           END-IF                                                         
139700         END-IF                                                           
139800                                                                          
139900         IF TEST-IDFOTNR(GRP-IX, 2) NOT = ZERO                            
140000           IF TEST-IDFOTNR(GRP-IX, 2) = TEST-IDFOTNR(GRP-IX, 3)           
140100             IF MID-IDFOTNR(GRP-IX, 3) NOT = ALL '+'                      
140200               MOVE MFS-NUM-FAELT-FEL TO                                  
140300                                      MOD-IDFOTNR-ATTR(GRP-IX, 3)         
140400               MOVE JA TO INDATA-FEL                                      
140500             ELSE                                                         
140600               MOVE MFS-NUM-FAELT-FEL TO                                  
140700                                      MOD-IDFOTNR-ATTR(GRP-IX, 2)         
140800               MOVE JA TO INDATA-FEL                                      
140900             END-IF                                                       
141000           END-IF                                                         
141100         END-IF                                                           
141200       ELSE                                                               
141300         IF TEST-IDFOTNR(GRP-IX, 2) NOT = ZERO                            
141400           IF TEST-IDFOTNR(GRP-IX, 2) = TEST-IDFOTNR(GRP-IX, 3)           
141500             IF MID-IDFOTNR(GRP-IX, 3) NOT = ALL '+'                      
141600               MOVE MFS-NUM-FAELT-FEL TO                                  
141700                                      MOD-IDFOTNR-ATTR(GRP-IX, 3)         
141800               MOVE JA TO INDATA-FEL                                      
141900             ELSE                                                         
142000               MOVE MFS-NUM-FAELT-FEL TO                                  
142100                                      MOD-IDFOTNR-ATTR(GRP-IX, 2)         
142200               MOVE JA TO INDATA-FEL                                      
142300             END-IF                                                       
142400           END-IF                                                         
142500         END-IF                                                           
142600       END-IF                                                             
142700     END-IF                                                               
142800     .                                                                    
142900     EJECT                                                                
143000 BE-KOLLA-RUB-MED-BAS SECTION.                                            
143100     SKIP2                                                                
143200     MOVE +1 TO INDX                                                      
143300     PERFORM UNTIL INDX >= +6                                             
143400       IF IN-IDRUBNR(INDX) NOT = ZERO                                     
143500         MOVE MID-IDRUBNR(INDX) TO W-IDRUBNR                              
143600         PERFORM IMS-GU-RUB                                               
143700         IF SEGMENT-FINNS                                                 
143800           IF (TEST-IDRUBNR(2) NOT = ZERO)                                
143900           OR (TEST-IDRUBNR(3) NOT = ZERO)                                
144000           OR (TEST-IDRUBNR(4) NOT = ZERO)                                
144100           OR (TEST-IDRUBNR(5) NOT = ZERO)                                
144200             IF RUB-RUB-FLKOMBINERAS = ' ' OR 'N'                         
144300               MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(INDX)           
144400               MOVE JA TO INDATA-FEL                                      
144500             END-IF                                                       
144600           END-IF                                                         
144700         ELSE                                                             
144800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(INDX)               
144900           MOVE JA TO INDATA-FEL                                          
145000         END-IF                                                           
145100       END-IF                                                             
145200       ADD +1 TO INDX                                                     
145300     END-PERFORM                                                          
145400     .                                                                    
145500     EJECT                                                                
145600 BF-KOLLA-FOT-MED-BAS SECTION.                                            
145700     SKIP2                                                                
145800     IF IDFOTNR-RAD4-FINNS = JA                                           
145900       MOVE +1 TO INDX                                                    
146000       PERFORM UNTIL INDX >= +4                                           
146100         IF  MID-IDFOTNR-RAD4(INDX) NUMERIC                               
146200         AND MID-IDFOTNR-RAD4(INDX) > ZERO                                
146300           MOVE MID-IDFOTNR-RAD4(INDX) TO W-IDFOTNR                       
146400           PERFORM IMS-GU-FOT                                             
146500           IF SEGMENT-SAKNAS                                              
146600             MOVE MFS-NUM-FAELT-FEL TO                                    
146700                                    MOD-IDFOTNR-RAD4-ATTR(INDX)           
146800             MOVE JA TO INDATA-FEL                                        
146900           END-IF                                                         
147000         END-IF                                                           
147100         ADD +1 TO INDX                                                   
147200       END-PERFORM                                                        
147300     END-IF                                                               
147400                                                                          
147500     IF IDFOTNR-FINNS = JA                                                
147600       MOVE +1 TO GRP-IX                                                  
147700       PERFORM UNTIL GRP-IX >= +6                                         
147800         MOVE +1 TO INDX                                                  
147900         PERFORM UNTIL  INDX >= +4                                        
148000           IF MID-IDFOTNR(GRP-IX, INDX) = ALL '+' OR ZERO                 
148100             CONTINUE                                                     
148200           ELSE                                                           
148300             MOVE MID-IDFOTNR(GRP-IX, INDX) TO W-IDFOTNR                  
148400             PERFORM IMS-GU-FOT                                           
148500             IF SEGMENT-SAKNAS                                            
148600               MOVE MFS-NUM-FAELT-FEL TO                                  
148700                                  MOD-IDFOTNR-ATTR(GRP-IX, INDX)          
148800               MOVE JA TO INDATA-FEL                                      
148900             END-IF                                                       
149000           END-IF                                                         
149100           ADD +1 TO INDX                                                 
149200         END-PERFORM                                                      
149300         ADD +1 TO GRP-IX                                                 
149400       END-PERFORM                                                        
149500     END-IF                                                               
149600     .                                                                    
149700     EJECT                                                                
149800 BG-FLYTTA-TEST-INBAS SECTION.                                            
149900     SKIP2                                                                
150000     IF TEST-FLRUBTYP = 'J'                                               
150100       MOVE TEST-IDRUBNR(1) TO INBAS-IDRUBNR(1)                           
150200       MOVE TEST-IDRUBNR(2) TO INBAS-IDRUBNR(2)                           
150300       MOVE TEST-IDRUBNR(3) TO INBAS-IDRUBNR(3)                           
150400     ELSE                                                                 
150500       MOVE TEST-IDRUBNR(1) TO INBAS-IDRUBNR(1)                           
150600       MOVE TEST-IDRUBNR(4) TO INBAS-IDRUBNR(2)                           
150700       MOVE TEST-IDRUBNR(5) TO INBAS-IDRUBNR(3)                           
150800     END-IF                                                               
150900     .                                                                    
151000     EJECT                                                                
151100 BH-NOLLSTALL-IN SECTION.                                                 
151200     SKIP2                                                                
151300     MOVE ZERO TO IN-IDRUBNR(1)                                           
151400                  IN-IDRUBNR(2)                                           
151500                  IN-IDRUBNR(3)                                           
151600                  IN-IDRUBNR(4)                                           
151700                  IN-IDRUBNR(5)                                           
151800                  IN-IDFOTNR-RAD4(1)                                      
151900                  IN-IDFOTNR-RAD4(2)                                      
152000                  IN-IDFOTNR-RAD4(3)                                      
152100     .                                                                    
152200     EJECT                                                                
152300 BI-KOLLA-MID-EJ-IFYLLD SECTION.                                          
152400                                                                          
152500     MOVE +1 TO INDX                                                      
152600     PERFORM UNTIL INDX >= +6                                             
152700       IF MID-IDRUBNR(INDX) NOT = ALL '+'                                 
152800          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRUBNR-ATTR(INDX)               
152900          MOVE JA TO INDATA-FEL                                           
153000       END-IF                                                             
153100       IF MID-TEKOL(INDX) NOT = ALL '+'                                   
153200          MOVE MFS-ALFA-FAELT-FEL TO MOD-TEKOL-ATTR(INDX)                 
153300          MOVE JA TO INDATA-FEL                                           
153400       END-IF                                                             
153500       IF MID-IDFOTNR(INDX, 1) NOT = ALL '+'                              
153600          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFOTNR-ATTR(INDX, 1)            
153700          MOVE JA TO INDATA-FEL                                           
153800       END-IF                                                             
153900       IF MID-IDFOTNR(INDX, 2) NOT = ALL '+'                              
154000          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFOTNR-ATTR(INDX, 2)            
154100          MOVE JA TO INDATA-FEL                                           
154200       END-IF                                                             
154300       IF MID-IDFOTNR(INDX, 3) NOT = ALL '+'                              
154400          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFOTNR-ATTR(INDX, 3)            
154500          MOVE JA TO INDATA-FEL                                           
154600       END-IF                                                             
154700       ADD +1 TO INDX                                                     
154800     END-PERFORM                                                          
154900                                                                          
155000     MOVE +1 TO INDX                                                      
155100     PERFORM UNTIL INDX >= +4                                             
155200       IF MID-IDFOTNR-RAD4(INDX) NOT = ALL '+'                            
155300          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFOTNR-RAD4-ATTR(INDX)          
155400          MOVE JA TO INDATA-FEL                                           
155500       END-IF                                                             
155600       ADD +1 TO INDX                                                     
155700     END-PERFORM                                                          
155800                                                                          
155900     IF MID-BERUBTEXT-1 NOT = ALL '+'                                     
156000        MOVE MFS-ALFA-FAELT-FEL TO MOD-BERUBTEXT-1-ATTR                   
156100        MOVE JA TO INDATA-FEL                                             
156200     END-IF                                                               
156300     IF MID-BERUBTEXT-2 NOT = ALL '+'                                     
156400        MOVE MFS-ALFA-FAELT-FEL TO MOD-BERUBTEXT-2-ATTR                   
156500        MOVE JA TO INDATA-FEL                                             
156600     END-IF                                                               
156700     IF MID-BERUBTEXT-3 NOT = ALL '+'                                     
156800        MOVE MFS-ALFA-FAELT-FEL TO MOD-BERUBTEXT-3-ATTR                   
156900        MOVE JA TO INDATA-FEL                                             
157000     END-IF                                                               
157100     IF MID-BERUBTEXT-4 NOT = ALL '+'                                     
157200        MOVE MFS-ALFA-FAELT-FEL TO MOD-BERUBTEXT-4-ATTR                   
157300        MOVE JA TO INDATA-FEL                                             
157400     END-IF                                                               
157500                                                                          
157600     IF MID-IDILLU NOT = ALL '+'                                          
157700        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDILLU-ATTR                        
157800        MOVE JA TO INDATA-FEL                                             
157900     END-IF                                                               
158000     .                                                                    
158100     EJECT                                                                
158200 BJ-KOLLA-BEHANDLING-KDCATPUB SECTION.                                    
158300******************************************************************        
158400* FUNKTIONER:                                                             
158500*  BORTTAG:                                                               
158600*  KOPIERING AV RADER:                                                    
158700*  UPPDATERING AV RADER:                                                  
158800*  NYUPPLÄGG AV RADER - NYTT AVSNITT                                      
158900*  NYUPPLÄGG AV RADER - EXISTERANDE AVSNITT                               
159000*  UPPDATERING AV KDCATPUB-FROM - EXISTERANDE RAD                         
159100******************************************************************        
159200                                                                          
159300     MOVE NEJ TO SW-BORTTAG                                               
159400                 SW-KOPIERING                                             
159500                 SW-UPPDATERING                                           
159600                 SW-NYUPPLAGG                                             
159700                 SW-NYUPPLAGG-NYTT                                        
159800                 SW-KDCATPUB-FROM                                         
159900                 SW-KDCATPUB-TOM                                          
160000                                                                          
160100     IF MID-BORT NOT = ALL '+'                                            
160200        IF MID-KDCATPUB-R-COPY = ALL '+' AND MID-KDCATPUB-R-FROM =        
160300           ALL '+' AND MID-KDCATPUB-R-TOM = ALL '+'                       
160400              MOVE JA TO SW-BORTTAG                                       
160500        ELSE                                                              
160600           IF MID-KDCATPUB-R-COPY NOT = ALL '+'                           
160700              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR         
160800           END-IF                                                         
160900           IF MID-KDCATPUB-R-FROM NOT = ALL '+'                           
161000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-FROM-ATTR         
161100           END-IF                                                         
161200           IF MID-KDCATPUB-R-TOM NOT = ALL '+'                            
161300              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR          
161400           END-IF                                                         
161500           MOVE JA TO INDATA-FEL                                          
161600        END-IF                                                            
161700     ELSE                                                                 
161800        IF MID-KDCATPUB-R-COPY NOT = ALL '+'                              
161900           IF MID-KDCATPUB-R-FROM = ALL '+'                               
162000              MOVE JA TO SW-KOPIERING                                     
162110              MOVE MID-KDCATPUB-R-COPY TO WS-KDCATPUB-R-AVV               
162120              PERFORM S50-Y2K-KDCATPUB-R                                  
162130              MOVE WS-KDCATPUB-AAAAVV  TO WS-KDCATPUB-COPY                
162200           ELSE                                                           
162300              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-FROM-ATTR         
162400              MOVE JA TO INDATA-FEL                                       
162500           END-IF                                                         
162600        ELSE                                                              
162700           IF MID-KDCATPUB-R-FROM NOT = ALL '+'                           
162800              MOVE MID-KDCATPUB-R-FROM TO WS-KDCATPUB-R-AVV               
162820              PERFORM S50-Y2K-KDCATPUB-R                                  
162830              MOVE WS-KDCATPUB-AAAAVV  TO WS-KDCATPUB-FROM                
162900              MOVE JA TO SW-KDCATPUB-FROM                                 
163000           END-IF                                                         
163100        END-IF                                                            
163200     END-IF                                                               
163300                                                                          
163400     IF MID-KDCATPUB-R-TOM NOT = ALL '+'                                  
163500        MOVE JA TO SW-KDCATPUB-TOM                                        
163600        MOVE MID-KDCATPUB-R-TOM TO WS-KDCATPUB-R-AVV                      
163620        PERFORM S50-Y2K-KDCATPUB-R                                        
163630        MOVE WS-KDCATPUB-AAAAVV TO WS-KDCATPUB-TOM                        
163700     END-IF                                                               
163800                                                                          
163900     IF INDATA-FEL = NEJ                                                  
164000        PERFORM IMS-GU-AVS                                                
164100        IF SEGMENT-SAKNAS                                                 
164200           MOVE JA TO SW-NYUPPLAGG-NYTT                                   
164300************ ev rättning av fel                                           
164400           MOVE NEJ TO SW-BORTTAG                                         
164500                       SW-KOPIERING                                       
164600                       SW-UPPDATERING                                     
164700                       SW-NYUPPLAGG                                       
164800                       SW-KDCATPUB-TOM                                    
164900                       SW-KDCATPUB-FROM                                   
165000************ ev rättning av fel                                           
165100        ELSE                                                              
165200           MOVE WS-KDCATPUB TO W-KDCATPUB                                 
165300           PERFORM BJB-SOEK-GALLANDE                                      
165400           IF GALLANDE-FINNS = JA                                         
165500              IF SW-BORTTAG = NEJ AND SW-KOPIERING = NEJ                  
165600                 AND SW-KDCATPUB-FROM = NEJ                               
165700                 MOVE JA TO SW-UPPDATERING                                
165800              END-IF                                                      
165900           ELSE                                                           
166000              IF SW-BORTTAG = NEJ AND SW-KOPIERING = NEJ                  
166100                 AND SW-KDCATPUB-FROM = NEJ                               
166200                 MOVE JA TO SW-NYUPPLAGG                                  
166300              END-IF                                                      
166400           END-IF                                                         
166500        END-IF                                                            
166600                                                                          
166700        IF SW-NYUPPLAGG-NYTT = JA                                         
166800           IF MID-KDCATPUB-R-TOM = ALL '+'                                
166900              MOVE HIGH-VALUE TO WS-KDCATPUB-TOM                          
167000           END-IF                                                         
167100        ELSE                                                              
167200           PERFORM BJC-LAS-HUVUD-TILL-TAB                                 
167300        END-IF                                                            
167400                                                                          
167500        IF SW-KOPIERING = JA                                              
167600           MOVE WS-KDCATPUB TO W-KDCATPUB                                 
167700           PERFORM BJB-SOEK-GALLANDE                                      
167800           IF GALLANDE-FINNS = NEJ                                        
167900              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR         
168000              MOVE JA TO INDATA-FEL                                       
168100           ELSE                                                           
168200              MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                         
168300              PERFORM BJB-SOEK-GALLANDE                                   
168400              IF GALLANDE-FINNS = JA                                      
168500               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR        
168600                 MOVE JA TO INDATA-FEL                                    
168700              ELSE                                                        
168800                 MOVE WS-KDCATPUB-COPY TO TEST-KDCATPUB-FROM              
168900                 PERFORM S02-KOLLA-GILTIG-KATPUB                          
169000                 IF GODK-KATPUB-FINNS = NEJ                               
169100                    MOVE MFS-ALFA-FAELT-FEL TO                            
169200                                 MOD-KDCATPUB-R-COPY-ATTR                 
169300                    MOVE JA TO INDATA-FEL                                 
169400                 ELSE                                                     
169500                    IF MID-KDCATPUB-R-TOM NOT = ALL '+'                   
169600                       MOVE WS-KDCATPUB-TOM TO TEST-KDCATPUB-TOM          
169700                       PERFORM S04-KOLLA-GILTIG-KATPUB                    
169800                       IF GODK-KATPUB-FINNS = NEJ                         
169900                          MOVE MFS-ALFA-FAELT-FEL                         
170000                                 TO MOD-KDCATPUB-R-TOM-ATTR               
170100                          MOVE JA TO INDATA-FEL                           
170200                       ELSE                                               
170300                          PERFORM S03-KOLLA-TABELL                        
170400                       END-IF                                             
170500                    ELSE                                                  
170600                       PERFORM S03-KOLLA-TABELL                           
170700                    END-IF                                                
170800                 END-IF                                                   
170900              END-IF                                                      
171000           END-IF                                                         
171100        END-IF                                                            
171200                                                                          
171300        IF SW-BORTTAG = JA                                                
171400           PERFORM BJA-KOLLA-SISTA-GALLANDE                               
171500           IF GALLANDE-FINNS = NEJ                                        
171600              MOVE MFS-ALFA-FAELT-FEL TO MOD-BORT-ATTR                    
171700              MOVE JA TO INDATA-FEL                                       
171800           ELSE                                                           
171900              PERFORM S03-KOLLA-TABELL                                    
172000           END-IF                                                         
172100        END-IF                                                            
172200                                                                          
172300        IF SW-NYUPPLAGG-NYTT = JA OR SW-NYUPPLAGG = JA                    
172400           IF MID-KDCATPUB-R-TOM = ALL '+'                                
172500              MOVE HIGH-VALUE TO WS-KDCATPUB-TOM                          
172600           ELSE                                                           
172710              MOVE MID-KDCATPUB-R-TOM TO WS-KDCATPUB-R-AVV                
172720              PERFORM S50-Y2K-KDCATPUB-R                                  
172730              MOVE WS-KDCATPUB-AAAAVV                                     
172740                                    TO WS-KDCATPUB-TOM                    
172800           END-IF                                                         
172900           MOVE WS-KDCATPUB TO W-KDCATPUB                                 
173000                               TEST-KDCATPUB-FROM                         
173100           PERFORM S02-KOLLA-GILTIG-KATPUB                                
173200           IF GODK-KATPUB-FINNS = NEJ                                     
173300              MOVE FEL-3(SPRAAK-IX) TO MOD-TEMFSINF                       
173400              MOVE JA TO INDATA-FEL                                       
173500           ELSE                                                           
173600              IF MID-KDCATPUB-R-TOM NOT = ALL '+'                         
173700                 MOVE WS-KDCATPUB-TOM TO TEST-KDCATPUB-TOM                
173800                 PERFORM S04-KOLLA-GILTIG-KATPUB                          
173900                 IF GODK-KATPUB-FINNS = NEJ                               
174000                    MOVE MFS-ALFA-FAELT-FEL TO                            
174100                                  MOD-KDCATPUB-R-TOM-ATTR                 
174200                    MOVE JA TO INDATA-FEL                                 
174300                 ELSE                                                     
174400                    PERFORM S03-KOLLA-TABELL                              
174500                 END-IF                                                   
174600              ELSE                                                        
174700                 PERFORM S03-KOLLA-TABELL                                 
174800              END-IF                                                      
174900           END-IF                                                         
175000        END-IF                                                            
175100                                                                          
175200        IF SW-KDCATPUB-FROM = JA                                          
175300           MOVE WS-KDCATPUB-FROM TO W-KDCATPUB                            
175400           PERFORM BJB-SOEK-GALLANDE                                      
175500           IF GALLANDE-FINNS = JA                                         
175600              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-FROM-ATTR         
175700              MOVE JA TO INDATA-FEL                                       
175800           ELSE                                                           
175900              MOVE WS-KDCATPUB-FROM TO TEST-KDCATPUB-FROM                 
176000              PERFORM S02-KOLLA-GILTIG-KATPUB                             
176100              IF GODK-KATPUB-FINNS = NEJ                                  
176200                 MOVE MFS-ALFA-FAELT-FEL TO                               
176300                             MOD-KDCATPUB-R-FROM-ATTR                     
176400                 MOVE JA TO INDATA-FEL                                    
176500              ELSE                                                        
176600                 PERFORM S03-KOLLA-TABELL                                 
176700              END-IF                                                      
176800           END-IF                                                         
176900        END-IF                                                            
177000                                                                          
177100        IF SW-UPPDATERING = JA                                            
177200          IF MID-KDCATPUB-R-TOM NOT = ALL '+'                             
177300            MOVE DAGENS-AAR-VECKA TO AKTUELLT-AAR                         
177400            MOVE WS-KDCATPUB-TOM TO TEST-KDCATPUB-TOM                     
177500            PERFORM S04-KOLLA-GILTIG-KATPUB                               
177600            IF GODK-KATPUB-FINNS = NEJ                                    
177700               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR         
177800               MOVE JA TO INDATA-FEL                                      
177900            ELSE                                                          
178000               PERFORM S03-KOLLA-TABELL                                   
178100            END-IF                                                        
178200          END-IF                                                          
178300        END-IF                                                            
178400     END-IF                                                               
178500     .                                                                    
178600     EJECT                                                                
178700 BJA-KOLLA-SISTA-GALLANDE SECTION.                                        
178800                                                                          
178900     MOVE NEJ TO GALLANDE-FINNS                                           
179000     PERFORM IMS-GU-AVS                                                   
179100     PERFORM IMS-GET-AVS-RAD                                              
179200     PERFORM UNTIL SEGMENT-SAKNAS OR RAD-IDCATRAD > 14                    
179300        IF RAD-IDCATRAD = ZERO                                            
179400          CONTINUE                                                        
179500        ELSE                                                              
179600           IF RAD-KDCATPUB-FOM = WS-KDCATPUB                              
179700              CONTINUE                                                    
179800           ELSE                                                           
179900              MOVE JA TO GALLANDE-FINNS                                   
180000           END-IF                                                         
180100        END-IF                                                            
180200        PERFORM IMS-GET-AVS-RAD                                           
180300     END-PERFORM                                                          
180400     .                                                                    
180500     EJECT                                                                
180600 BJB-SOEK-GALLANDE SECTION.                                               
180700                                                                          
180800     MOVE NEJ TO GALLANDE-FINNS                                           
180900     MOVE +1 TO W-IDCATRAD                                                
181000     PERFORM IMS-GHNP-AVS-RAD-FIRST                                       
181100     PERFORM UNTIL W-IDCATRAD > 14 OR (GALLANDE-FINNS = JA)               
181200        IF SEGMENT-FINNS                                                  
181300           MOVE JA TO GALLANDE-FINNS                                      
181400           MOVE RAD-KDCATPUB-TOM TO FIXAD-KDCATPUB-TOM                    
181500        END-IF                                                            
181600        ADD +1 TO W-IDCATRAD                                              
181700        PERFORM IMS-GHNP-AVS-RAD                                          
181800     END-PERFORM                                                          
181900     .                                                                    
182000     EJECT                                                                
182100 BJC-LAS-HUVUD-TILL-TAB SECTION.                                          
182200                                                                          
182300******** NOLLSTÄLL TABELLER                                               
182400                                                                          
182500     MOVE +1 TO TAB-IX                                                    
182600     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
182700        MOVE SPACE TO TAB1-KDCATPUB-FROM(TAB-IX)                          
182800                      TAB1-KDCATPUB-TOM(TAB-IX)                           
182900        ADD +1 TO TAB-IX                                                  
183000     END-PERFORM                                                          
183100                                                                          
183200     MOVE +1 TO TAB-IX2                                                   
183300     PERFORM UNTIL TAB-IX2 > TAB-IX-MAX                                   
183400        MOVE SPACE TO TAB2-KDCATPUB-FROM(TAB-IX2)                         
183500                      TAB2-KDCATPUB-TOM(TAB-IX2)                          
183600        ADD +1 TO TAB-IX2                                                 
183700     END-PERFORM                                                          
183800                                                                          
183900******** LAS TILL TABELL                                                  
184000                                                                          
184100     MOVE LOW-VALUE TO W-KDCATPUB-MIN                                     
184200     MOVE HIGH-VALUE TO W-KDCATPUB-MAX                                    
184300     MOVE +1 TO W-IDCATRAD-MIN                                            
184400     MOVE +14 TO W-IDCATRAD-MAX                                           
184500     MOVE ZERO TO TAB-IX2                                                 
184600                                                                          
184700     PERFORM IMS-GNP-AVS-RAD-SOEK-FIRST                                   
184800     PERFORM UNTIL SEGMENT-SAKNAS                                         
184900        MOVE +1 TO TAB-IX                                                 
185000        MOVE NEJ TO TAB-FINNS                                             
185100        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
185200           IF RAD-KDCATPUB-FOM = TAB1-KDCATPUB-FROM(TAB-IX)               
185300              MOVE JA TO TAB-FINNS                                        
185400              MOVE +99 TO TAB-IX                                          
185500           ELSE                                                           
185600              ADD +1 TO TAB-IX                                            
185700           END-IF                                                         
185800        END-PERFORM                                                       
185900        IF TAB-FINNS = NEJ                                                
186000           ADD +1 TO TAB-IX2                                              
186100           MOVE RAD-KDCATPUB-FOM TO TAB1-KDCATPUB-FROM(TAB-IX2)           
186200           MOVE RAD-KDCATPUB-TOM  TO TAB1-KDCATPUB-TOM(TAB-IX2)           
186300        END-IF                                                            
186400        PERFORM IMS-GNP-AVS-RAD-SOEK                                      
186500     END-PERFORM                                                          
186600                                                                          
186700******** SORTERA TABELL                                                   
186800                                                                          
186900     MOVE +6      TO STEGLAANGD                                           
187000     MOVE TAB-IX2 TO ANTAL                                                
187100     MOVE +6      TO NYCKELLAANGD                                         
187200     CALL WINTSOR USING TABELL STEGLAANGD ANTAL                           
187300     TAB1-KDCATPUB-FROM(1) NYCKELLAANGD                                   
187400                                                                          
187500******** FLYTTA TILL TABELL2                                              
187600                                                                          
187700     MOVE +1 TO TAB-IX                                                    
187800     MOVE +2 TO TAB-IX2                                                   
187900     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
188000        MOVE TAB1-KDCATPUB-FROM(TAB-IX) TO                                
188100                    TAB2-KDCATPUB-FROM(TAB-IX2)                           
188200        MOVE TAB1-KDCATPUB-TOM(TAB-IX) TO                                 
188300                    TAB2-KDCATPUB-TOM(TAB-IX2)                            
188400        ADD +1 TO TAB-IX                                                  
188500        ADD +2 TO TAB-IX2                                                 
188600     END-PERFORM                                                          
188700     .                                                                    
188800     EJECT                                                                
188900 C-ISRT-AVS SECTION.                                                      
189000     SKIP2                                                                
189100     MOVE LOW-VALUE TO W-KDCATPUB                                         
189200     MOVE NEJ TO NYUPPLAGG-NOLLRAD                                        
189300     MOVE W-IDCATNR  TO W-IDCATNR-WDN1                                    
189400     PERFORM IMS-GU-KAT                                                   
189500     IF SEGMENT-FINNS                                                     
189600       PERFORM CA-NYUPPLAGG-NOLL-AVS                                      
189700       PERFORM CB-NYUPPLAGG-ETT-AVS                                       
189800                                                                          
189900       MOVE W-IDCATNR    TO AVS-IDCATNR                                   
190000       MOVE W-IDCATGRP   TO AVS-IDCATGRP                                  
190100       MOVE KEY-IDCATAVS TO W-IDCATAVS                                    
190200       MOVE W-IDCATAVS   TO AVS-IDCATAVS                                  
190300       MOVE ZERO         TO AVS-IDVERS                                    
190500*      --- tillfällig fix tills Reine A meddelar annat.                   
190600*      MOVE JA           TO AVS-FLAVSTVAD                                 
190800       MOVE NEJ          TO AVS-FLAVSTVAD                                 
190900       MOVE JA           TO AVS-FLAVSUST                                  
191000       IF W-IDCATAVS > +1                                                 
191100         PERFORM IMS-ISRT-AVS                                             
191200         MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                            
191300       END-IF                                                             
191400       MOVE JA TO NYUPPLAGG-NOLLRAD                                       
191500     ELSE                                                                 
191600       MOVE FEL-4 (SPRAAK-IX) TO MOD-TEMFSFEL                             
191700       MOVE JA TO INDATA-FEL                                              
191800     END-IF                                                               
191900     .                                                                    
192000     EJECT                                                                
192100 CA-NYUPPLAGG-NOLL-AVS SECTION.                                           
192200     SKIP2                                                                
192300     MOVE ZERO TO W-IDCATAVS                                              
192400     PERFORM IMS-GHU-AVS                                                  
192500     IF SEGMENT-SAKNAS                                                    
192600       MOVE W-IDCATNR     TO AVS-IDCATNR                                  
192700       MOVE W-IDCATGRP    TO AVS-IDCATGRP                                 
192800       MOVE ZERO          TO AVS-IDCATAVS                                 
192900       MOVE ZERO          TO AVS-IDVERS                                   
193000       MOVE NEJ           TO AVS-FLAVSTVAD                                
193100       MOVE NEJ           TO AVS-FLAVSUST                                 
193200       PERFORM IMS-ISRT-AVS                                               
193300                                                                          
193400       MOVE ZERO              TO RAD-IDCATRAD                             
193500       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
193600       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
193700       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
193800       MOVE 'N'               TO RAD-KDRADST                              
193900       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
193910       MOVE 'FRÅN  CA-NYUPPLAGG-NOLL-AVS SECTION.'                        
193920                         TO ABENDINFO                                     
194000       PERFORM IMS-ISRT-AVS-RAD                                           
194100     ELSE                                                                 
194200       PERFORM CAA-NYUPPLAGG-NOLL-RAD                                     
194300     END-IF                                                               
194400     .                                                                    
194500     EJECT                                                                
194600 CAA-NYUPPLAGG-NOLL-RAD SECTION.                                          
194700     SKIP2                                                                
194800     MOVE ZERO TO W-IDCATRAD                                              
194900     PERFORM IMS-GHNP-AVS-RAD                                             
195000     IF SEGMENT-SAKNAS                                                    
195100       MOVE W-IDCATRAD        TO RAD-IDCATRAD                             
195200       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
195300       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
195400       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
195500       MOVE 'N'               TO RAD-KDRADST                              
195600       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
195610       MOVE 'FRÅN  CAA-NYUPPLAGG-NOLL-RAD SECTION.'                       
195620                         TO ABENDINFO                                     
195700       PERFORM IMS-ISRT-AVS-RAD                                           
195800     END-IF                                                               
195900     .                                                                    
196000     EJECT                                                                
196100 CB-NYUPPLAGG-ETT-AVS SECTION.                                            
196200     SKIP2                                                                
196300     MOVE +1 TO W-IDCATAVS                                                
196400     PERFORM IMS-GHU-AVS                                                  
196500     IF SEGMENT-SAKNAS                                                    
196600       MOVE W-IDCATNR     TO AVS-IDCATNR                                  
196700       MOVE W-IDCATGRP    TO AVS-IDCATGRP                                 
196800       MOVE +1            TO AVS-IDCATAVS                                 
196900       MOVE ZERO          TO AVS-IDVERS                                   
197000       MOVE NEJ           TO AVS-FLAVSTVAD                                
197100       MOVE NEJ           TO AVS-FLAVSUST                                 
197200       PERFORM IMS-ISRT-AVS                                               
197300                                                                          
197400       MOVE ZERO              TO RAD-IDCATRAD                             
197500       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
197600       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
197700       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
197800       MOVE 'N'               TO RAD-KDRADST                              
197900       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
197910       MOVE 'FRÅN  CB-NYUPPLAGG-ETT-AVS SECTION. .'                       
197920                         TO ABENDINFO                                     
198000       PERFORM IMS-ISRT-AVS-RAD                                           
198100     ELSE                                                                 
198200       PERFORM CBA-NYUPPLAGG-ETT-RAD                                      
198300     END-IF                                                               
198400     .                                                                    
198500     EJECT                                                                
198600 CBA-NYUPPLAGG-ETT-RAD SECTION.                                           
198700     SKIP2                                                                
198800     MOVE ZERO TO W-IDCATRAD                                              
198900     PERFORM IMS-GHNP-AVS-RAD                                             
199000     IF SEGMENT-SAKNAS                                                    
199100       MOVE W-IDCATRAD        TO RAD-IDCATRAD                             
199200       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
199300       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
199400       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
199500       MOVE 'N'               TO RAD-KDRADST                              
199600       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
199610       MOVE 'FRÅN  CBA-NYUPPLAGG-ETT-RAD SECTION. '                       
199620                         TO ABENDINFO                                     
199700       PERFORM IMS-ISRT-AVS-RAD                                           
199800     END-IF                                                               
199900     .                                                                    
200000     EJECT                                                                
200100 D-UPPDATERA SECTION.                                                     
200200                                                                          
200300     PERFORM IMS-GHU-AVS                                                  
200400                                                                          
200500     IF MID-IDILLU NOT = ALL '+'                                          
200600       PERFORM DA-UPPDAT-ILLU                                             
200700     END-IF                                                               
200800                                                                          
200900     IF NYUPPLAGG-NOLLRAD = JA                                            
201000       PERFORM DE-NYUPPLAGG-NOLL-RAD                                      
201100     END-IF                                                               
201200                                                                          
201300     MOVE WS-KDCATPUB TO W-KDCATPUB                                       
201400                                                                          
201500     IF IDRUBNR-FINNS = JA                                                
201600       PERFORM DB-UPPDAT-RAD1                                             
201700     END-IF                                                               
201800                                                                          
201900     IF  MID-BERUBTEXT-1 = ALL '+'                                        
202000     AND MID-BERUBTEXT-2 = ALL '+'                                        
202100     AND IDFOTNR-RAD4-FINNS = NEJ                                         
202200       CONTINUE                                                           
202300     ELSE                                                                 
202400       PERFORM DC-UPPDAT-RAD4                                             
202500     END-IF                                                               
202600                                                                          
202700     IF  MID-BERUBTEXT-3 = ALL '+'                                        
202800     AND MID-BERUBTEXT-4 = ALL '+'                                        
202900       CONTINUE                                                           
203000     ELSE                                                                 
203100       PERFORM DC--UPPDAT-VADIS-RAD4                                      
203200*      --- EGENTLIGEN RAD5                                                
203300     END-IF                                                               
203400                                                                          
203500     IF TEKOL-FINNS = JA                                                  
203600     OR IDFOTNR-FINNS = JA                                                
203700       PERFORM DD-UPPDAT-RAD10-14                                         
203800     END-IF                                                               
203900                                                                          
204000     IF UPPDATE-FLAGGA = JA                                               
204100       PERFORM IMS-GHU-AVS                                                
204200       IF SEGMENT-FINNS                                                   
204300         MOVE JA TO AVS-FLAVSUST                                          
204400         PERFORM IMS-REPL-AVS                                             
204500         MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF (1:20)                     
204600                                                                          
204700         IF AVS-FLAVSTVAD = JA                                            
204800           IF ( IDRUBNR-FINNS    = JA                                     
204900           OR BERUBTEXT-FINNS    = JA                                     
205000           OR TEKOL-FINNS        = JA                                     
205100           OR IDFOTNR-FINNS      = JA                                     
205200           OR IDFOTNR-RAD4-FINNS = JA )                                   
205300             MOVE FRAGA-1(SPRAAK-IX) TO MOD-TEMFSFEL                      
205400           END-IF                                                         
205500         END-IF                                                           
205600       END-IF                                                             
205700     END-IF                                                               
205800                                                                          
205900     IF SW-UPPDATERING = JA                                               
206000        IF MID-KDCATPUB-R-TOM = ALL '+'                                   
206100           CONTINUE                                                       
206200        ELSE                                                              
206300           PERFORM DF-UPPDAT-KDCATPUB-TOM                                 
206400           PERFORM S11-OMNUM-KDCATPUB-FROM                                
206500           MOVE MED-9(SPRAAK-IX) TO MOD-TEMFSINF                          
206600        END-IF                                                            
206700     ELSE                                                                 
206800        IF SW-NYUPPLAGG = JA                                              
206900           PERFORM S10-OMNUM-KDCATPUB-TOM                                 
207000           PERFORM S11-OMNUM-KDCATPUB-FROM                                
207100           MOVE MED-9(SPRAAK-IX) TO MOD-TEMFSINF                          
207200        END-IF                                                            
207300     END-IF                                                               
207400                                                                          
207500     MOVE WS-KDCATPUB TO GALLANDE-KDCATPUB                                
207600     .                                                                    
207700     EJECT                                                                
207800 DA-UPPDAT-ILLU SECTION.                                                  
207900     SKIP2                                                                
208000     MOVE NEJ TO GALLANDE-ILLU-FINNS                                      
208100     PERFORM IMS-GHNP-AVS-ILLU                                            
208200     PERFORM UNTIL SEGMENT-SAKNAS                                         
208300       IF ILLU-KDCATPUB-FOM = WS-KDCATPUB                                 
208400          IF MID-IDILLU = ZERO                                            
208500            PERFORM IMS-DLET-AVS                                          
208600            MOVE JA TO UPPDATE-FLAGGA                                     
208700                       GALLANDE-ILLU-FINNS                                
208800          ELSE                                                            
208900            IF MID-IDILLU = ILLU-IDILLU                                   
209000              MOVE JA TO GALLANDE-ILLU-FINNS                              
209100            ELSE                                                          
209200              PERFORM IMS-DLET-AVS                                        
209300              MOVE MID-IDILLU  TO ILLU-IDILLU                             
209400              MOVE WS-KDCATPUB TO ILLU-KDCATPUB-FOM                       
209500              PERFORM IMS-ISRT-AVS-ILLU                                   
209600              MOVE JA TO UPPDATE-FLAGGA                                   
209700                         GALLANDE-ILLU-FINNS                              
209800            END-IF                                                        
209900          END-IF                                                          
210000       END-IF                                                             
210100       PERFORM IMS-GHNP-AVS-ILLU                                          
210200     END-PERFORM                                                          
210300     IF GALLANDE-ILLU-FINNS = NEJ                                         
210400       IF MID-IDILLU = ZERO                                               
210500         CONTINUE                                                         
210600       ELSE                                                               
210610         IF (W-IDCATGRP NOT = 20 and 30 and 40 and 50 and 60              
210620                          and 70 and 80 and 90)                           
210630         AND (W-IDCATAVS = 0001)                                          
210631            CONTINUE                                                      
210632*           ---   0001-avsnitt får inte ha ILLU utom på X0-grp            
210640         ELSE                                                             
210700           MOVE MID-IDILLU TO ILLU-IDILLU                                 
210800           MOVE WS-KDCATPUB TO ILLU-KDCATPUB-FOM                          
210900           PERFORM IMS-ISRT-AVS-ILLU                                      
211000           MOVE JA TO UPPDATE-FLAGGA                                      
211100         END-IF                                                           
211110       END-IF                                                             
211200     END-IF                                                               
211300     .                                                                    
211400     EJECT                                                                
211500 DB-UPPDAT-RAD1 SECTION.                                                  
211600     SKIP2                                                                
211700     MOVE +1 TO W-IDCATRAD                                                
211800     PERFORM S01-GET-AVS-RAD                                              
211900     PERFORM DBA-UPPDAT-RAD1-RUB                                          
212000     .                                                                    
212100     EJECT                                                                
212200 DBA-UPPDAT-RAD1-RUB SECTION.                                             
212300     SKIP2                                                                
212400     PERFORM IMS-GHNP-AVS-RUB                                             
212500     PERFORM UNTIL SEGMENT-SAKNAS                                         
212600       IF RUB-IDSEGMNR = 1                                                
212700         MOVE +1 TO INDX                                                  
212800         PERFORM DBAA-UPPDATE-RUB                                         
212900       ELSE                                                               
213000         IF RUB-IDSEGMNR = 2                                              
213100           MOVE +2 TO INDX                                                
213200           PERFORM DBAA-UPPDATE-RUB                                       
213300         ELSE                                                             
213400           IF RUB-IDSEGMNR = 3                                            
213500             MOVE +3 TO INDX                                              
213600             PERFORM DBAA-UPPDATE-RUB                                     
213700           END-IF                                                         
213800         END-IF                                                           
213900       END-IF                                                             
214000       PERFORM IMS-GHNP-AVS-RUB                                           
214100     END-PERFORM                                                          
214200     IF INBAS-IDRUBNR(1) NOT = ZERO                                       
214300       MOVE +1 TO INDX                                                    
214400       PERFORM DBAB-UPPDATE-RUB-NY                                        
214500     END-IF                                                               
214600     IF INBAS-IDRUBNR(2) NOT = ZERO                                       
214700       MOVE +2 TO INDX                                                    
214800       PERFORM DBAB-UPPDATE-RUB-NY                                        
214900     END-IF                                                               
215000     IF INBAS-IDRUBNR(3) NOT = ZERO                                       
215100       MOVE +3 TO INDX                                                    
215200       PERFORM DBAB-UPPDATE-RUB-NY                                        
215300     END-IF                                                               
215400     .                                                                    
215500     EJECT                                                                
215600 DBAA-UPPDATE-RUB SECTION.                                                
215700     SKIP2                                                                
215800     IF INBAS-IDRUBNR(INDX) = ZERO                                        
215900       PERFORM IMS-DLET-AVS                                               
216000       MOVE JA TO UPPDATE-FLAGGA                                          
216100     ELSE                                                                 
216200       IF INBAS-IDRUBNR(INDX) = RUB-IDRUBNR                               
216300         IF RUB-FLRUBTYP NOT = TEST-FLRUBTYP                              
216400           PERFORM IMS-DLET-AVS                                           
216500           MOVE JA TO UPPDATE-FLAGGA                                      
216600         END-IF                                                           
216700       ELSE                                                               
216800         PERFORM IMS-DLET-AVS                                             
216900         MOVE JA TO UPPDATE-FLAGGA                                        
217000       END-IF                                                             
217100     END-IF                                                               
217200     .                                                                    
217300     EJECT                                                                
217400 DBAB-UPPDATE-RUB-NY SECTION.                                             
217500     SKIP2                                                                
217600     IF INBAS-IDRUBNR(INDX) = BAS-IDRUBNR(INDX)                           
217700       IF BAS-FLRUBTYP NOT = TEST-FLRUBTYP                                
217800         MOVE INBAS-IDRUBNR(INDX) TO RUB-IDRUBNR                          
217900         MOVE INDX                TO RUB-IDSEGMNR                         
218000         MOVE TEST-FLRUBTYP       TO RUB-FLRUBTYP                         
218100         PERFORM IMS-ISRT-AVS-RUB                                         
218200         MOVE JA TO UPPDATE-FLAGGA                                        
218300       END-IF                                                             
218400     ELSE                                                                 
218500       MOVE INBAS-IDRUBNR(INDX) TO RUB-IDRUBNR                            
218600       MOVE INDX                TO RUB-IDSEGMNR                           
218700       MOVE TEST-FLRUBTYP       TO RUB-FLRUBTYP                           
218800       PERFORM IMS-ISRT-AVS-RUB                                           
218900       MOVE JA TO UPPDATE-FLAGGA                                          
219000     END-IF                                                               
219100     .                                                                    
219200     EJECT                                                                
219300 DC-UPPDAT-RAD4 SECTION.                                                  
219400     SKIP2                                                                
219500     MOVE +4 TO W-IDCATRAD                                                
219600     PERFORM S01-GET-AVS-RAD                                              
219700                                                                          
219800     IF MID-BERUBTEXT-1 = ALL '+' AND                                     
219900     MID-BERUBTEXT-2 = ALL '+'                                            
220000       CONTINUE                                                           
220100     ELSE                                                                 
220200       PERFORM DCA-UPPDAT-BERUBTEXT                                       
220300     END-IF                                                               
220400     IF IDFOTNR-RAD4-FINNS = JA                                           
220500       PERFORM DCB-UPPDAT-IDFOTNR-RAD4                                    
220600     END-IF                                                               
220700     .                                                                    
220800     EJECT                                                                
220900 DC--UPPDAT-VADIS-RAD4 SECTION.                                           
221000     SKIP2                                                                
221100     MOVE +5 TO W-IDCATRAD                                                
221200     PERFORM S01-GET-AVS-RAD                                              
221300                                                                          
221400     PERFORM IMS-GHNP-AVS-TEXT                                            
221500                                                                          
221600     IF MID-BERUBTEXT-3 NOT = ALL '+'                                     
221700       MOVE MID-BERUBTEXT-3      TO TEXT-BERUBTEXT-1                      
221800     END-IF                                                               
221900     IF MID-BERUBTEXT-4 NOT = ALL '+'                                     
222000       MOVE MID-BERUBTEXT-4      TO TEXT-BERUBTEXT-2                      
222100     END-IF                                                               
222200                                                                          
222300*    --- VADIS SKALL KUNNA HA BLANK "RAD4" PÅ RAD5                        
222400*    --- ELLER EN VADIS-SPECIFIK TEXT. OM RAD5 SAKNAS, TAR VADIS          
222500*    --- DEN VANLIGA RAD4.                                                
222600                                                                          
222700     IF SEGMENT-FINNS                                                     
222800         PERFORM IMS-REPL-AVS                                             
222900         MOVE JA TO UPPDATE-FLAGGA                                        
223000     ELSE                                                                 
223100         PERFORM IMS-ISRT-AVS-TEXT                                        
223200         MOVE JA TO UPPDATE-FLAGGA                                        
223300     END-IF                                                               
223400     .                                                                    
223500     EJECT                                                                
223600 DCA-UPPDAT-BERUBTEXT SECTION.                                            
223700     SKIP2                                                                
223800     PERFORM IMS-GHNP-AVS-TEXT                                            
223900                                                                          
224000     IF SEGMENT-FINNS                                                     
224100       IF MID-BERUBTEXT-1 = SPACE AND                                     
224200       MID-BERUBTEXT-2 = SPACE                                            
224300         PERFORM IMS-DLET-AVS                                             
224400         MOVE JA TO UPPDATE-FLAGGA                                        
224500       ELSE                                                               
224600         IF MID-BERUBTEXT-1 NOT = ALL '+'                                 
224700           MOVE MID-BERUBTEXT-1  TO TEXT-BERUBTEXT-1                      
224800         END-IF                                                           
224900         IF MID-BERUBTEXT-2 NOT = ALL '+'                                 
225000           MOVE MID-BERUBTEXT-2  TO TEXT-BERUBTEXT-2                      
225100         END-IF                                                           
225200         IF TEXT-BERUBTEXT-1 = SPACE AND                                  
225300         TEXT-BERUBTEXT-2 = SPACE                                         
225400           PERFORM IMS-DLET-AVS                                           
225500           MOVE JA TO UPPDATE-FLAGGA                                      
225600         ELSE                                                             
225700           PERFORM IMS-REPL-AVS                                           
225800           MOVE JA TO UPPDATE-FLAGGA                                      
225900         END-IF                                                           
226000       END-IF                                                             
226100     ELSE                                                                 
226200       IF  MID-BERUBTEXT-1 = SPACE                                        
226300       AND MID-BERUBTEXT-2 = SPACE                                        
226400         CONTINUE                                                         
226500       ELSE                                                               
226600         IF MID-BERUBTEXT-1 = ALL '+'                                     
226700           MOVE SPACE              TO TEXT-BERUBTEXT-1                    
226800         ELSE                                                             
226900           MOVE MID-BERUBTEXT-1    TO TEXT-BERUBTEXT-1                    
227000         END-IF                                                           
227100         IF MID-BERUBTEXT-2 = ALL '+'                                     
227200           MOVE SPACE              TO TEXT-BERUBTEXT-2                    
227300         ELSE                                                             
227400           MOVE MID-BERUBTEXT-2    TO TEXT-BERUBTEXT-2                    
227500         END-IF                                                           
227600         PERFORM IMS-ISRT-AVS-TEXT                                        
227700         MOVE JA TO UPPDATE-FLAGGA                                        
227800       END-IF                                                             
227900     END-IF                                                               
228000     .                                                                    
228100     EJECT                                                                
228200 DCB-UPPDAT-IDFOTNR-RAD4 SECTION.                                         
228300     SKIP2                                                                
228400     PERFORM IMS-GHNP-AVS-FOT                                             
228500     PERFORM UNTIL                                                        
228600      NOT ( SEGMENT-FINNS )                                               
228700       IF FOT-IDSEGMNR = 1                                                
228800         MOVE +1 TO INDX                                                  
228900         PERFORM DCBA-UPPDATE-FOT                                         
229000       ELSE                                                               
229100         IF FOT-IDSEGMNR = 2                                              
229200           MOVE +2 TO INDX                                                
229300           PERFORM DCBA-UPPDATE-FOT                                       
229400         ELSE                                                             
229500           IF FOT-IDSEGMNR = 3                                            
229600             MOVE +3 TO INDX                                              
229700             PERFORM DCBA-UPPDATE-FOT                                     
229800           END-IF                                                         
229900         END-IF                                                           
230000       END-IF                                                             
230100       PERFORM IMS-GHNP-AVS-FOT                                           
230200     END-PERFORM                                                          
230300     IF TEST-IDFOTNR-RAD4(1) NOT = ZERO                                   
230400       MOVE +1 TO INDX                                                    
230500       PERFORM DCBB-UPPDATE-FOT-NY                                        
230600     END-IF                                                               
230700     IF TEST-IDFOTNR-RAD4(2) NOT = ZERO                                   
230800       MOVE +2 TO INDX                                                    
230900       PERFORM DCBB-UPPDATE-FOT-NY                                        
231000     END-IF                                                               
231100     IF TEST-IDFOTNR-RAD4(3) NOT = ZERO                                   
231200       MOVE +3 TO INDX                                                    
231300       PERFORM DCBB-UPPDATE-FOT-NY                                        
231400     END-IF                                                               
231500     .                                                                    
231600     EJECT                                                                
231700 DCBA-UPPDATE-FOT SECTION.                                                
231800     SKIP2                                                                
231900     IF TEST-IDFOTNR-RAD4(INDX) = ZERO                                    
232000       PERFORM IMS-DLET-AVS                                               
232100       MOVE JA TO UPPDATE-FLAGGA                                          
232200     ELSE                                                                 
232300       IF TEST-IDFOTNR-RAD4(INDX) = FOT-IDFOTNR                           
232400         CONTINUE                                                         
232500       ELSE                                                               
232600         PERFORM IMS-DLET-AVS                                             
232700         MOVE JA TO UPPDATE-FLAGGA                                        
232800       END-IF                                                             
232900     END-IF                                                               
233000     .                                                                    
233100     EJECT                                                                
233200 DCBB-UPPDATE-FOT-NY SECTION.                                             
233300     SKIP2                                                                
233400     IF TEST-IDFOTNR-RAD4(INDX) = BAS-IDFOTNR-RAD4(INDX)                  
233500       CONTINUE                                                           
233600     ELSE                                                                 
233700       MOVE TEST-IDFOTNR-RAD4(INDX) TO FOT-IDFOTNR                        
233800       MOVE INDX               TO FOT-IDSEGMNR                            
233900       PERFORM IMS-ISRT-AVS-FOT                                           
234000       MOVE JA TO UPPDATE-FLAGGA                                          
234100     END-IF                                                               
234200     .                                                                    
234300     EJECT                                                                
234400 DD-UPPDAT-RAD10-14 SECTION.                                              
234500     SKIP2                                                                
234600     MOVE +1 TO GRP-IX                                                    
234700     PERFORM UNTIL GRP-IX >= +6                                           
234800       MOVE NEJ TO UPPDAT-TEKOL                                           
234900       MOVE NEJ TO UPPDAT-IDFOTNR                                         
235000                                                                          
235100       IF MID-TEKOL(GRP-IX) = ALL '+'                                     
235200         CONTINUE                                                         
235300       ELSE                                                               
235400         MOVE JA TO UPPDAT-TEKOL                                          
235500       END-IF                                                             
235600                                                                          
235700       IF IDFOTNR-FINNS = JA                                              
235800         IF (MID-IDFOTNR(GRP-IX, 1) NOT = ALL '+')                        
235900         OR (MID-IDFOTNR(GRP-IX, 2) NOT = ALL '+')                        
236000         OR (MID-IDFOTNR(GRP-IX, 3) NOT = ALL '+')                        
236100           MOVE JA TO UPPDAT-IDFOTNR                                      
236200         END-IF                                                           
236300       END-IF                                                             
236400                                                                          
236500       IF UPPDAT-TEKOL = JA OR                                            
236600       UPPDAT-IDFOTNR = JA                                                
236700                                                                          
236800         EVALUATE GRP-IX                                                  
236900         WHEN 1                                                           
237000           MOVE 10 TO W-IDCATRAD                                          
237100         WHEN 2                                                           
237200           MOVE 11 TO W-IDCATRAD                                          
237300         WHEN 3                                                           
237400           MOVE 12 TO W-IDCATRAD                                          
237500         WHEN 4                                                           
237600           MOVE 13 TO W-IDCATRAD                                          
237700         WHEN 5                                                           
237800           MOVE 14 TO W-IDCATRAD                                          
237900         END-EVALUATE                                                     
238000                                                                          
238100         PERFORM S01-GET-AVS-RAD                                          
238200                                                                          
238300         IF UPPDAT-TEKOL = JA                                             
238400           PERFORM DDA-UPPDAT-TEKOL                                       
238500         END-IF                                                           
238600         IF UPPDAT-IDFOTNR = JA                                           
238700           PERFORM DDB-UPPDAT-IDFOTNR                                     
238800         END-IF                                                           
238900       END-IF                                                             
239000       ADD +1 TO GRP-IX                                                   
239100     END-PERFORM                                                          
239200     .                                                                    
239300     EJECT                                                                
239400 DDA-UPPDAT-TEKOL SECTION.                                                
239500     SKIP2                                                                
239600     PERFORM IMS-GHNP-AVS-TEXT                                            
239700     IF SEGMENT-FINNS                                                     
239800       IF MID-TEKOL(GRP-IX) = SPACE                                       
239900          PERFORM IMS-DLET-AVS                                            
240000          MOVE JA TO UPPDATE-FLAGGA                                       
240100       ELSE                                                               
240110         MOVE SPACE             TO TEXT-BERUBTEXT                         
240200         MOVE MID-TEKOL(GRP-IX) TO TEXT-TEKOL                             
240300         PERFORM IMS-REPL-AVS                                             
240400         MOVE JA TO UPPDATE-FLAGGA                                        
240500       END-IF                                                             
240600     ELSE                                                                 
240610       MOVE SPACE             TO TEXT-BERUBTEXT                           
240700       MOVE MID-TEKOL(GRP-IX) TO TEXT-TEKOL                               
240800       PERFORM IMS-ISRT-AVS-TEXT                                          
240900       MOVE JA TO UPPDATE-FLAGGA                                          
241000     END-IF                                                               
241100     .                                                                    
241200     EJECT                                                                
241300 DDB-UPPDAT-IDFOTNR SECTION.                                              
241400     SKIP2                                                                
241500     PERFORM IMS-GHNP-AVS-FOT                                             
241600     PERFORM UNTIL                                                        
241700      NOT ( SEGMENT-FINNS )                                               
241800       IF FOT-IDSEGMNR = 1                                                
241900         MOVE +1 TO INDX                                                  
242000         PERFORM DDBA-UPPDATE-FOT                                         
242100       ELSE                                                               
242200         IF FOT-IDSEGMNR = 2                                              
242300           MOVE +2 TO INDX                                                
242400           PERFORM DDBA-UPPDATE-FOT                                       
242500         ELSE                                                             
242600           IF FOT-IDSEGMNR = 3                                            
242700             MOVE +3 TO INDX                                              
242800             PERFORM DDBA-UPPDATE-FOT                                     
242900           END-IF                                                         
243000         END-IF                                                           
243100       END-IF                                                             
243200       PERFORM IMS-GHNP-AVS-FOT                                           
243300     END-PERFORM                                                          
243400     IF TEST-IDFOTNR(GRP-IX, 1) NOT = ZERO                                
243500       MOVE +1 TO INDX                                                    
243600       PERFORM DDBB-UPPDATE-FOT-NY                                        
243700     END-IF                                                               
243800     IF TEST-IDFOTNR(GRP-IX, 2) NOT = ZERO                                
243900       MOVE +2 TO INDX                                                    
244000       PERFORM DDBB-UPPDATE-FOT-NY                                        
244100     END-IF                                                               
244200     IF TEST-IDFOTNR(GRP-IX, 3) NOT = ZERO                                
244300       MOVE +3 TO INDX                                                    
244400       PERFORM DDBB-UPPDATE-FOT-NY                                        
244500     END-IF                                                               
244600     .                                                                    
244700     EJECT                                                                
244800 DDBA-UPPDATE-FOT SECTION.                                                
244900     SKIP2                                                                
245000     IF TEST-IDFOTNR(GRP-IX, INDX) = ZERO                                 
245100       PERFORM IMS-DLET-AVS                                               
245200       MOVE JA TO UPPDATE-FLAGGA                                          
245300     ELSE                                                                 
245400       IF TEST-IDFOTNR(GRP-IX, INDX) = FOT-IDFOTNR                        
245500         CONTINUE                                                         
245600       ELSE                                                               
245700         PERFORM IMS-DLET-AVS                                             
245800         MOVE JA TO UPPDATE-FLAGGA                                        
245900       END-IF                                                             
246000     END-IF                                                               
246100     .                                                                    
246200     EJECT                                                                
246300 DDBB-UPPDATE-FOT-NY SECTION.                                             
246400     SKIP2                                                                
246500     IF TEST-IDFOTNR(GRP-IX, INDX) = BAS-IDFOTNR(GRP-IX, INDX)            
246600       CONTINUE                                                           
246700     ELSE                                                                 
246800       MOVE TEST-IDFOTNR(GRP-IX, INDX) TO FOT-IDFOTNR                     
246900       MOVE INDX               TO FOT-IDSEGMNR                            
247000       PERFORM IMS-ISRT-AVS-FOT                                           
247100       MOVE JA TO UPPDATE-FLAGGA                                          
247200     END-IF                                                               
247300     .                                                                    
247400     EJECT                                                                
247500 DE-NYUPPLAGG-NOLL-RAD SECTION.                                           
247600     SKIP2                                                                
247700     MOVE ZERO TO W-IDCATRAD                                              
247800     PERFORM IMS-GHNP-AVS-RAD                                             
247900                                                                          
248000     IF SEGMENT-SAKNAS                                                    
248100       MOVE W-IDCATRAD        TO RAD-IDCATRAD                             
248200       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
248300       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
248400       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
248500       MOVE 'N'               TO RAD-KDRADST                              
248600       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
248610       MOVE 'FRÅN  DE-NYUPPLAGG-NOLL-RAD SECTION. '                       
248620                         TO ABENDINFO                                     
248700       PERFORM IMS-ISRT-AVS-RAD                                           
248800     END-IF                                                               
248900     .                                                                    
249000     EJECT                                                                
249100 DF-UPPDAT-KDCATPUB-TOM SECTION.                                          
249200                                                                          
249300     PERFORM IMS-GHU-AVS                                                  
249400     MOVE WS-KDCATPUB TO W-KDCATPUB                                       
249500     MOVE +1 TO W-IDCATRAD                                                
249600     PERFORM IMS-GHNP-AVS-RAD                                             
249700     PERFORM UNTIL W-IDCATRAD > 14                                        
249800        IF SEGMENT-FINNS                                                  
249900           MOVE WS-KDCATPUB-TOM TO RAD-KDCATPUB-TOM                       
250000           PERFORM IMS-REPL-AVS                                           
250100        END-IF                                                            
250200        ADD +1 TO W-IDCATRAD                                              
250300        PERFORM IMS-GHNP-AVS-RAD                                          
250400     END-PERFORM                                                          
250401                                                                          
250410*    WDN513 VADIS-VARIANTNYCKLAR LIKADANT                                 
250420     MOVE WS-KDCATPUB TO W-KDCATPUB-F13                                   
250430                                                                          
250440     PERFORM IMS-GHNP-AVS-VADIS                                           
250450     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
250460        MOVE WS-KDCATPUB-TOM TO VADIS-KDCATPUB-TOM                        
250470        PERFORM IMS-REPL-AVS                                              
250480        PERFORM IMS-GHNP-AVS-VADIS                                        
250490     END-PERFORM                                                          
250500     .                                                                    
250600     EJECT                                                                
250700 E-LAS-KATALOGRAD SECTION.                                                
250800     SKIP2                                                                
250900******************************************************************        
251000*    RAD 1-14 SAMT ILLU HAR ALLTID SAMMA KDCATPUB                         
251100******************************************************************        
251200                                                                          
251300     PERFORM IMS-GU-AVS                                                   
251400     IF SEGMENT-SAKNAS                                                    
251500       MOVE FEL-6 (SPRAAK-IX) TO MOD-TEMFSFEL                             
251600       MOVE JA TO INDATA-FEL                                              
251700     ELSE                                                                 
251800       MOVE NEJ TO GALLANDE-ILLU                                          
251900       PERFORM IMS-GNP-AVS-ILLU                                           
252000       PERFORM UNTIL SEGMENT-SAKNAS OR GALLANDE-ILLU = JA                 
252100          IF ILLU-KDCATPUB-FOM = GALLANDE-KDCATPUB                        
252200             MOVE ILLU-IDILLU TO MOD-IDILLU                               
252300             MOVE JA TO GALLANDE-ILLU                                     
252400          ELSE                                                            
252500             PERFORM IMS-GNP-AVS-ILLU                                     
252600          END-IF                                                          
252700       END-PERFORM                                                        
252800       IF SEGMENT-SAKNAS OR (GALLANDE-ILLU = NEJ)                         
252900         MOVE ZERO TO MOD-IDILLU                                          
253000       END-IF                                                             
253100                                                                          
253200       MOVE GALLANDE-KDCATPUB TO W-KDCATPUB                               
253300       MOVE GALLANDE-KDCATPUB (4:3)                                       
253310                              TO MOD-KDCATPUB-R-UT                        
253400       PERFORM EA-LAS-RAD-0-1                                             
253500       PERFORM EB-LAS-RAD4                                                
253600       PERFORM EC-LAS-RAD-10-14                                           
253700       PERFORM EE-SOEK-NASTA                                              
253800     END-IF                                                               
253900     .                                                                    
254000     EJECT                                                                
254100 EA-LAS-RAD-0-1 SECTION.                                                  
254200******************************************************************        
254300*    ALLA 0-RADER HAR KDCATPUB BLANK OBEROENDE AV AVSNITT                 
254400*    ALLA KATGRP MÅSTE HA AVSNITT 0                                       
254500*    ALLA KATGRP MÅSTE HA AVSNITT 1                                       
254600*    ALLA AVSNITT MÅSTE HA 0-RAD                                          
254700******************************************************************        
254800     SKIP2                                                                
254900     MOVE MFS-RENSA-FAELT TO MOD-IDRUBNR(1)                               
255000                             MOD-IDRUBNR(2)                               
255100                             MOD-IDRUBNR(3)                               
255200                             MOD-IDRUBNR(4)                               
255300                             MOD-IDRUBNR(5)                               
255500     MOVE +0 TO W-IDCATRAD                                                
255600     MOVE LOW-VALUE TO W-KDCATPUB                                         
255610                                                                          
255620     MOVE W-WDN501KY-X  TO W-WDN5GSEQ-01-X                                
255630     MOVE W-WDN512KY-X  TO W-WDN5GSEQ-12-X                                
255700     PERFORM IMS-GU-AVSG-HAEN                                             
255800                                                                          
255900     IF SEGMENT-FINNS                                                     
256000       IF MOD-TEMFSINF (1:20) > SPACE                                     
256100         MOVE MED-2 (SPRAAK-IX) TO MOD-TEMFSINF(21:34)                    
256200       ELSE                                                               
256300         MOVE MED-2 (SPRAAK-IX) TO MOD-TEMFSINF                           
256400       END-IF                                                             
256500     END-IF                                                               
256600                                                                          
256700     MOVE GALLANDE-KDCATPUB TO W-KDCATPUB                                 
256800                                                                          
256900     MOVE +1 TO W-IDCATRAD                                                
257000     PERFORM IMS-GNP-AVS-RAD                                              
257100                                                                          
257200     IF SEGMENT-FINNS                                                     
257300       MOVE RAD-KDCATPUB-TOM (4:3)                                        
257310                             TO MOD-KDCATPUB-R-TOM                        
257400       IF RAD-KDCATPUB-TOM = HIGH-VALUE                                   
257500          MOVE '999' TO MOD-KDCATPUB-R-TOM                                
257600       END-IF                                                             
257700       MOVE ZERO TO SPAR-IDRUBNR(1)                                       
257800                    SPAR-IDRUBNR(2)                                       
257900                    SPAR-IDRUBNR(3)                                       
258000       MOVE 'J' TO IN-FLRUBTYP                                            
258100                                                                          
258200       PERFORM IMS-GNP-AVS-RUB                                            
258300       MOVE +1 TO INDX                                                    
258400       PERFORM UNTIL SEGMENT-SAKNAS OR INDX >= +4                         
258500         MOVE RUB-IDRUBNR  TO SPAR-IDRUBNR(RUB-IDSEGMNR)                  
258600         MOVE RUB-FLRUBTYP TO IN-FLRUBTYP                                 
258700         PERFORM IMS-GNP-AVS-RUB                                          
258800         ADD +1 TO INDX                                                   
258900       END-PERFORM                                                        
259000                                                                          
259100       IF SPAR-IDRUBNR(1) NOT = ZERO                                      
259200         MOVE SPAR-IDRUBNR(1) TO MOD-IDRUBNR(1)                           
259300       END-IF                                                             
259400       IF IN-FLRUBTYP = 'J'                                               
259500         IF SPAR-IDRUBNR(2) NOT = ZERO                                    
259600           MOVE SPAR-IDRUBNR(2) TO MOD-IDRUBNR(2)                         
259700         END-IF                                                           
259800         IF SPAR-IDRUBNR(3) NOT = ZERO                                    
259900           MOVE SPAR-IDRUBNR(3) TO MOD-IDRUBNR(3)                         
260000         END-IF                                                           
260100       ELSE                                                               
260200         IF SPAR-IDRUBNR(2) NOT = ZERO                                    
260300           MOVE SPAR-IDRUBNR(2) TO MOD-IDRUBNR(4)                         
260400         END-IF                                                           
260500         IF SPAR-IDRUBNR(3) NOT = ZERO                                    
260600           MOVE SPAR-IDRUBNR(3) TO MOD-IDRUBNR(5)                         
260700         END-IF                                                           
260800       END-IF                                                             
260900     END-IF                                                               
261000     .                                                                    
261100     EJECT                                                                
261200 EB-LAS-RAD4 SECTION.                                                     
261300     SKIP2                                                                
261400     MOVE +4 TO W-IDCATRAD                                                
261500     PERFORM IMS-GNP-AVS-RAD                                              
261600                                                                          
261700     IF SEGMENT-FINNS                                                     
261800*      MOVE RAD-KDCATPUB-TOM (4:3)                                        
261810*                            TO MOD-KDCATPUB-R-TOM                        
261900*      IF RAD-KDCATPUB-TOM = HIGH-VALUE                                   
262000*         MOVE '999' TO MOD-KDCATPUB-R-TOM                                
262100*      END-IF                                                             
262200       PERFORM EBA-LAS-BERUBTEXT                                          
262300       PERFORM EBB-LAS-IDFOTNR-RAD4                                       
262400     ELSE                                                                 
262500       MOVE MFS-RENSA-FAELT TO MOD-BERUBTEXT-1                            
262600                               MOD-BERUBTEXT-2                            
262700                               MOD-IDFOTNR-RAD4(1)                        
262800                               MOD-IDFOTNR-RAD4(2)                        
262900                               MOD-IDFOTNR-RAD4(3)                        
263000     END-IF                                                               
263100*                                                                         
263200*    --- LÄS RAD 5, SOM ÄR "RAD4" FÖR VADIS                               
263300*                                                                         
263400     MOVE +5 TO W-IDCATRAD                                                
263500     PERFORM IMS-GNP-AVS-RAD                                              
263600                                                                          
263700     IF SEGMENT-FINNS                                                     
263800*      MOVE RAD-KDCATPUB-TOM (4:3)                                        
263810*                            TO MOD-KDCATPUB-R-TOM                        
263900*      IF RAD-KDCATPUB-TOM = HIGH-VALUE                                   
264000*         MOVE '999' TO MOD-KDCATPUB-R-TOM                                
264100*      END-IF                                                             
264200       PERFORM EBC-LAS-BERUBTEXT-VADIS                                    
264300     ELSE                                                                 
264400       MOVE MOD-BERUBTEXT-1 TO MOD-BERUBTEXT-3                            
264500       MOVE MOD-BERUBTEXT-2 TO MOD-BERUBTEXT-4                            
264600     END-IF                                                               
264700     .                                                                    
264800     EJECT                                                                
264900 EBA-LAS-BERUBTEXT SECTION.                                               
265000     SKIP2                                                                
265100     PERFORM IMS-GNP-AVS-TEXT                                             
265200                                                                          
265300     IF SEGMENT-FINNS                                                     
265400       MOVE TEXT-BERUBTEXT-1 TO MOD-BERUBTEXT-1                           
265500       MOVE TEXT-BERUBTEXT-2 TO MOD-BERUBTEXT-2                           
265600     ELSE                                                                 
265700       MOVE MFS-RENSA-FAELT  TO MOD-BERUBTEXT-1                           
265800                                MOD-BERUBTEXT-2                           
265900     END-IF                                                               
266000     .                                                                    
266100     EJECT                                                                
266200 EBB-LAS-IDFOTNR-RAD4 SECTION.                                            
266300     SKIP2                                                                
266400     MOVE +1 TO INDX                                                      
266500                                                                          
266600     PERFORM IMS-GNP-AVS-FOT                                              
266700     PERFORM UNTIL SEGMENT-SAKNAS OR  INDX >= +4                          
266800       MOVE FOT-IDFOTNR  TO MOD-IDFOTNR-RAD4(FOT-IDSEGMNR)                
266900       ADD +1 TO INDX                                                     
267000       PERFORM IMS-GNP-AVS-FOT                                            
267100     END-PERFORM                                                          
267200     .                                                                    
267300     EJECT                                                                
267400 EBC-LAS-BERUBTEXT-VADIS SECTION.                                         
267500     SKIP2                                                                
267600     PERFORM IMS-GNP-AVS-TEXT                                             
267700                                                                          
267800     IF SEGMENT-FINNS                                                     
267900       MOVE TEXT-BERUBTEXT-1 TO MOD-BERUBTEXT-3                           
268000       MOVE TEXT-BERUBTEXT-2 TO MOD-BERUBTEXT-4                           
268100     ELSE                                                                 
268200       MOVE MOD-BERUBTEXT-1  TO MOD-BERUBTEXT-3                           
268300       MOVE MOD-BERUBTEXT-2  TO MOD-BERUBTEXT-4                           
268400     END-IF                                                               
268500     .                                                                    
268600     EJECT                                                                
268700 EC-LAS-RAD-10-14 SECTION.                                                
268800     SKIP2                                                                
268900     MOVE +10 TO W-IDCATRAD                                               
269000     MOVE +1  TO GRP-IX                                                   
269100     PERFORM IMS-GNP-AVS-RAD                                              
269200                                                                          
269300     PERFORM UNTIL  W-IDCATRAD >= +15                                     
269400       IF SEGMENT-FINNS                                                   
269500*        MOVE RAD-KDCATPUB-TOM (4:3)                                      
269510*                              TO MOD-KDCATPUB-R-TOM                      
269600*        IF MOD-KDCATPUB-R-TOM = HIGH-VALUE                               
269700*           MOVE '999' TO MOD-KDCATPUB-R-TOM                              
269800*        END-IF                                                           
269900         PERFORM ECA-LAS-TEKOL                                            
270000         PERFORM ECB-LAS-IDFOTNR                                          
270100         PERFORM ECD-LAS-REF                                              
270200       ELSE                                                               
270300         PERFORM ECC-BLANKA                                               
270400       END-IF                                                             
270500       ADD +1 TO W-IDCATRAD                                               
270600       ADD +1 TO GRP-IX                                                   
270700       PERFORM IMS-GNP-AVS-RAD                                            
270800     END-PERFORM                                                          
270900     .                                                                    
271000     EJECT                                                                
271100 ECA-LAS-TEKOL SECTION.                                                   
271200     SKIP2                                                                
271300     PERFORM IMS-GNP-AVS-TEXT                                             
271400                                                                          
271500     IF SEGMENT-FINNS                                                     
271600       MOVE TEXT-TEKOL TO MOD-TEKOL(GRP-IX)                               
271700     ELSE                                                                 
271800       MOVE MFS-RENSA-FAELT TO MOD-TEKOL(GRP-IX)                          
271900     END-IF                                                               
272000     .                                                                    
272100     EJECT                                                                
272200 ECB-LAS-IDFOTNR SECTION.                                                 
272300     SKIP2                                                                
272400     MOVE MFS-RENSA-FAELT TO MOD-IDFOTNR(GRP-IX, 1)                       
272500                             MOD-IDFOTNR(GRP-IX, 2)                       
272600                             MOD-IDFOTNR(GRP-IX, 3)                       
272700     MOVE +1 TO INDX                                                      
272800                                                                          
272900     PERFORM IMS-GNP-AVS-FOT                                              
273000     PERFORM UNTIL SEGMENT-SAKNAS OR INDX >= +4                           
273100       MOVE FOT-IDFOTNR  TO                                               
273200            MOD-IDFOTNR(GRP-IX, FOT-IDSEGMNR)                             
273300       ADD +1 TO INDX                                                     
273400       PERFORM IMS-GNP-AVS-FOT                                            
273500     END-PERFORM                                                          
273600     .                                                                    
273700     EJECT                                                                
273800 ECC-BLANKA SECTION.                                                      
273900     SKIP2                                                                
274000     MOVE MFS-RENSA-FAELT TO MOD-TEKOL(GRP-IX)                            
274100                             MOD-IDFOTNR(GRP-IX, 1)                       
274200                             MOD-IDFOTNR(GRP-IX, 2)                       
274300                             MOD-IDFOTNR(GRP-IX, 3)                       
274400     .                                                                    
274500     EJECT                                                                
274600 ECD-LAS-REF SECTION.                                                     
274700                                                                          
274710     MOVE W-WDN501KY-X  TO W-WDN5GSEQ-01-X                                
274720     MOVE W-WDN512KY-X  TO W-WDN5GSEQ-12-X                                
274800     PERFORM IMS-GU-AVSG-HAEN                                             
274900                                                                          
275000     IF SEGMENT-FINNS                                                     
275100       IF MOD-TEMFSINF (1:32) > SPACE                                     
275200         CONTINUE                                                         
275300*        MOVE med-2 (SPRAAK-IX) TO MOD-TEMFSINF(21:34)                    
275400       ELSE                                                               
275500         MOVE MED-2 (SPRAAK-IX) TO MOD-TEMFSINF                           
275600       END-IF                                                             
275700     END-IF                                                               
275800     .                                                                    
275900     EJECT                                                                
276000 EE-SOEK-NASTA SECTION.                                                   
276100                                                                          
276200     MOVE NEJ TO NASTA-FINNS                                              
276300     MOVE W-KDCATPUB TO W-KDCATPUB-MIN                                    
276400     MOVE HIGH-VALUE TO W-KDCATPUB-MAX                                    
276500     MOVE +1 TO W-IDCATRAD-MIN                                            
276600                W-IDCATRAD-MAX                                            
276700                                                                          
276800     PERFORM IMS-GNP-AVS-RAD-NASTA-FIRST                                  
276900     PERFORM UNTIL W-IDCATRAD-MIN > 14 OR (NASTA-FINNS = JA)              
277000        IF SEGMENT-FINNS                                                  
277100           MOVE RAD-KDCATPUB-FOM (4:3) TO MOD-KDCATPUB-R-NEXT             
277101                                                                          
277200           IF MFS-UPDATE                                                  
277300              CONTINUE                                                    
277400           ELSE                                                           
277500*             MOVE MED-3(SPRAAK-IX) TO MOD-TEMFSINF                       
277600              IF MOD-TEMFSINF (1:32) > SPACE                              
277700                MOVE MED-3 (SPRAAK-IX) TO MOD-TEMFSINF(34:21)             
277800              ELSE                                                        
277900                MOVE MED-3 (SPRAAK-IX) TO MOD-TEMFSINF                    
278000              END-IF                                                      
278100           END-IF                                                         
278200           MOVE JA TO NASTA-FINNS                                         
278300        END-IF                                                            
278400        ADD +1 TO W-IDCATRAD-MIN                                          
278500                  W-IDCATRAD-MAX                                          
278600        PERFORM IMS-GNP-AVS-RAD-NASTA                                     
278700     END-PERFORM                                                          
278800     IF NASTA-FINNS = NEJ                                                 
278900        MOVE MFS-RENSA-FAELT TO MOD-KDCATPUB-R-NEXT                       
279000     END-IF                                                               
279100     .                                                                    
279200     EJECT                                                                
279300 F-VISA-BILD-IGEN SECTION.                                                
279400     SKIP2                                                                
279500     MOVE +1 TO INDX                                                      
279600     PERFORM UNTIL INDX >= +6                                             
279700       MOVE MFS-ROER-EJ-FAELT TO MOD-IDRUBNR(INDX)                        
279800                                MOD-TEKOL(INDX)                           
279900                                MOD-IDFOTNR(INDX, 1)                      
280000                                MOD-IDFOTNR(INDX, 2)                      
280100                                MOD-IDFOTNR(INDX, 3)                      
280200       ADD +1 TO INDX                                                     
280300     END-PERFORM                                                          
280400     MOVE +1 TO INDX                                                      
280500     PERFORM UNTIL INDX >= +4                                             
280600       MOVE MFS-ROER-EJ-FAELT TO MOD-IDFOTNR-RAD4(INDX)                   
280700       ADD +1 TO INDX                                                     
280800     END-PERFORM                                                          
280900     MOVE MFS-ROER-EJ-FAELT TO MOD-BERUBTEXT-1                            
281000                               MOD-BERUBTEXT-2                            
281100                               MOD-BERUBTEXT-3                            
281200                               MOD-BERUBTEXT-4                            
281300                               MOD-IDILLU                                 
281400                               MOD-BORT                                   
281500                               MOD-KDCATPUB-R-COPY                        
281600                               MOD-KDCATPUB-R-FROM                        
281700                               MOD-KDCATPUB-R-TOM-IN                      
281800                               MOD-KDCATPUB-R-TOM                         
281900     .                                                                    
282000     EJECT                                                                
282100 G-RENSA-BILD SECTION.                                                    
282200     SKIP2                                                                
282300     MOVE +1 TO INDX                                                      
282400     PERFORM UNTIL INDX >= +6                                             
282500       MOVE MFS-RENSA-FAELT TO MOD-IDRUBNR(INDX)                          
282600                               MOD-TEKOL(INDX)                            
282700                               MOD-IDFOTNR(INDX, 1)                       
282800                               MOD-IDFOTNR(INDX, 2)                       
282900                               MOD-IDFOTNR(INDX, 3)                       
283000       ADD +1 TO INDX                                                     
283100     END-PERFORM                                                          
283200     MOVE +1 TO INDX                                                      
283300     PERFORM UNTIL INDX >= +4                                             
283400       MOVE MFS-RENSA-FAELT TO MOD-IDFOTNR-RAD4(INDX)                     
283500       ADD +1 TO INDX                                                     
283600     END-PERFORM                                                          
283700     MOVE MFS-RENSA-FAELT TO MOD-BERUBTEXT-1                              
283800                             MOD-BERUBTEXT-2                              
283900                             MOD-BERUBTEXT-3                              
284000                             MOD-BERUBTEXT-4                              
284100                             MOD-BORT                                     
284200                             MOD-IDILLU                                   
284300                             MOD-KDCATPUB-R-COPY                          
284400                             MOD-KDCATPUB-R-FROM                          
284500                             MOD-KDCATPUB-R-TOM                           
284600                             MOD-KDCATPUB-R-TOM-IN                        
284700     .                                                                    
284800     EJECT                                                                
284900 H-SKAPA-X0-ILLU SECTION.                                                 
285000     SKIP2                                                                
285100     MOVE LOW-VALUE TO W-KDCATPUB                                         
285200     IF SEGMENT-SAKNAS                                                    
285300       MOVE NEJ TO NYUPPLAGG-NOLLRAD                                      
285400       MOVE W-IDCATNR TO W-IDCATNR-WDN1                                   
285500       PERFORM IMS-GU-KAT                                                 
285600       IF SEGMENT-FINNS                                                   
285700         PERFORM HA-NYUPPLAGG-NOLL-AVS                                    
285800         PERFORM HB-NYUPPLAGG-ETT-AVS                                     
285900                                                                          
286000         MOVE W-IDCATNR    TO AVS-IDCATNR                                 
286100         MOVE W-IDCATGRP   TO AVS-IDCATGRP                                
286200         MOVE KEY-IDCATAVS TO W-IDCATAVS                                  
286300         MOVE W-IDCATAVS   TO AVS-IDCATAVS                                
286400         MOVE ZERO         TO AVS-IDVERS                                  
286600*        --- tillfälligt fix tills Reine A meddelar annat.                
286700*        MOVE JA           TO AVS-FLAVSTVAD                               
286900         MOVE NEJ          TO AVS-FLAVSTVAD                               
287000         MOVE JA           TO AVS-FLAVSUST                                
287100         MOVE JA TO NYUPPLAGG-NOLLRAD                                     
287200         MOVE MED-1 (SPRAAK-IX) TO MOD-TEMFSINF (1:20)                    
287300       ELSE                                                               
287400         MOVE FEL-4 (SPRAAK-IX) TO MOD-TEMFSFEL                           
287500         MOVE JA TO INDATA-FEL                                            
287600       END-IF                                                             
287700     ELSE                                                                 
287800       PERFORM IMS-GHNP-AVS-ILLU                                          
287900       IF SEGMENT-FINNS                                                   
288000          PERFORM IMS-DLET-AVS                                            
288100       END-IF                                                             
288200       MOVE MID-IDILLU  TO ILLU-IDILLU                                    
288300       MOVE LOW-VALUE   TO ILLU-KDCATPUB-FOM                              
288400       PERFORM IMS-ISRT-AVS-ILLU                                          
288500                                                                          
288600       MOVE MED-1 (SPRAAK-IX) TO MOD-TEMFSINF (1:20)                      
288700     END-IF                                                               
288800     .                                                                    
288900     EJECT                                                                
289000 HA-NYUPPLAGG-NOLL-AVS SECTION.                                           
289100     SKIP2                                                                
289200     MOVE ZERO TO W-IDCATAVS                                              
289300     PERFORM IMS-GHU-AVS                                                  
289400     IF SEGMENT-SAKNAS                                                    
289500       MOVE W-IDCATNR     TO AVS-IDCATNR                                  
289600       MOVE W-IDCATGRP    TO AVS-IDCATGRP                                 
289700       MOVE ZERO          TO AVS-IDCATAVS                                 
289800       MOVE ZERO          TO AVS-IDVERS                                   
289900       MOVE NEJ           TO AVS-FLAVSTVAD                                
290000       MOVE NEJ           TO AVS-FLAVSUST                                 
290100       PERFORM IMS-ISRT-AVS                                               
290200                                                                          
290300       MOVE MID-IDILLU   TO ILLU-IDILLU                                   
290400       MOVE LOW-VALUE    TO ILLU-KDCATPUB-FOM                             
290500       PERFORM IMS-ISRT-AVS-ILLU                                          
290600                                                                          
290700       MOVE ZERO              TO RAD-IDCATRAD                             
290800       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
290900       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
291000       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
291100       MOVE 'N'               TO RAD-KDRADST                              
291200       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
291210       MOVE 'FRÅN  HA-NYUPPLAGG-NOLL-AVS SECTION. '                       
291220                         TO ABENDINFO                                     
291300       PERFORM IMS-ISRT-AVS-RAD                                           
291400     ELSE                                                                 
291500       PERFORM HAA-NYUPPLAGG-NOLL-RAD                                     
291600     END-IF                                                               
291700     .                                                                    
291800     EJECT                                                                
291900 HAA-NYUPPLAGG-NOLL-RAD SECTION.                                          
292000     SKIP2                                                                
292100     PERFORM IMS-GHNP-AVS-ILLU                                            
292200     IF SEGMENT-FINNS                                                     
292300        PERFORM IMS-DLET-AVS                                              
292400     END-IF                                                               
292500     MOVE MID-IDILLU  TO ILLU-IDILLU                                      
292600     MOVE LOW-VALUE   TO ILLU-KDCATPUB-FOM                                
292700     PERFORM IMS-ISRT-AVS-ILLU                                            
292800     MOVE MED-1 (SPRAAK-IX) TO MOD-TEMFSINF (1:20)                        
292900                                                                          
293000     MOVE ZERO TO W-IDCATRAD                                              
293100     PERFORM IMS-GHNP-AVS-RAD                                             
293200     IF SEGMENT-SAKNAS                                                    
293300       MOVE W-IDCATRAD        TO RAD-IDCATRAD                             
293400       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
293500       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
293600       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
293700       MOVE 'N'               TO RAD-KDRADST                              
293800       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
293810       MOVE 'FRÅN  HAA-NYUPPLAGG-NOLL-RAD SECTION.'                       
293820                         TO ABENDINFO                                     
293900       PERFORM IMS-ISRT-AVS-RAD                                           
294000     END-IF                                                               
294100     .                                                                    
294200     EJECT                                                                
294300 HB-NYUPPLAGG-ETT-AVS SECTION.                                            
294400     SKIP2                                                                
294500     MOVE +1 TO W-IDCATAVS                                                
294600     PERFORM IMS-GHU-AVS                                                  
294700     IF SEGMENT-SAKNAS                                                    
294800       MOVE W-IDCATNR     TO AVS-IDCATNR                                  
294900       MOVE W-IDCATGRP    TO AVS-IDCATGRP                                 
295000       MOVE +1            TO AVS-IDCATAVS                                 
295100       MOVE ZERO          TO AVS-IDVERS                                   
295200       MOVE NEJ           TO AVS-FLAVSTVAD                                
295300       MOVE NEJ           TO AVS-FLAVSUST                                 
295400       PERFORM IMS-ISRT-AVS                                               
295500                                                                          
295600       MOVE ZERO              TO RAD-IDCATRAD                             
295700       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
295800       MOVE 'N'               TO RAD-KDRADST                              
295900       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
296000       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
296100       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
296110       MOVE 'FRÅN  HB-NYUPPLAGG-ETT-AVS SECTION.'                         
296120                         TO ABENDINFO                                     
296200       PERFORM IMS-ISRT-AVS-RAD                                           
296300     ELSE                                                                 
296400       PERFORM HBA-NYUPPLAGG-ETT-RAD                                      
296500     END-IF                                                               
296600     .                                                                    
296700     EJECT                                                                
296800 HBA-NYUPPLAGG-ETT-RAD SECTION.                                           
296900     SKIP2                                                                
297000     MOVE ZERO TO W-IDCATRAD                                              
297100     PERFORM IMS-GHNP-AVS-RAD                                             
297200     IF SEGMENT-SAKNAS                                                    
297300       MOVE W-IDCATRAD        TO RAD-IDCATRAD                             
297400       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
297500       MOVE 'N'               TO RAD-KDRADST                              
297600       MOVE LOW-VALUE         TO RAD-KDCATPUB-FOM                         
297700       MOVE HIGH-VALUE        TO RAD-KDCATPUB-TOM                         
297800       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
297810       MOVE 'FRÅN  HBA-NYUPPLAGG-ETT-RAD SECTION.      '                  
297820                         TO ABENDINFO                                     
297900       PERFORM IMS-ISRT-AVS-RAD                                           
298000     END-IF                                                               
298100     .                                                                    
298200     EJECT                                                                
298300 J-BORTTAG SECTION.                                                       
298400                                                                          
298500     PERFORM IMS-GU-AVS                                                   
298600     MOVE WS-KDCATPUB TO W-KDCATPUB                                       
298700                                                                          
298800     PERFORM IMS-GHNP-AVS-ILLU                                            
298900     PERFORM UNTIL SEGMENT-SAKNAS                                         
299000       IF ILLU-KDCATPUB-FOM = WS-KDCATPUB                                 
299100          PERFORM IMS-DLET-AVS                                            
299200       END-IF                                                             
299300       PERFORM IMS-GHNP-AVS-ILLU                                          
299400     END-PERFORM                                                          
299500                                                                          
299600     MOVE +1 TO W-IDCATRAD                                                
299700     PERFORM IMS-GHNP-AVS-RAD                                             
299800     IF SEGMENT-FINNS                                                     
299900*    -- Behöver inte ta bort underseg då logiska koppl ej finns           
300500        PERFORM IMS-DLET-AVS                                              
300600     END-IF                                                               
300700                                                                          
300800     MOVE +4 TO W-IDCATRAD                                                
300900     PERFORM IMS-GHNP-AVS-RAD                                             
301000     IF SEGMENT-FINNS                                                     
301010*    -- Behöver inte ta bort underseg då logiska koppl ej finns           
302200        PERFORM IMS-DLET-AVS                                              
302300     END-IF                                                               
302400                                                                          
302500     MOVE +5 TO W-IDCATRAD                                                
302600     PERFORM IMS-GHNP-AVS-RAD                                             
302700     IF SEGMENT-FINNS                                                     
302710*    -- Behöver inte ta bort underseg då logiska koppl ej finns           
303300        PERFORM IMS-DLET-AVS                                              
303400     END-IF                                                               
303500                                                                          
303600     MOVE +10 TO W-IDCATRAD                                               
303700     PERFORM IMS-GHNP-AVS-RAD                                             
303800     PERFORM UNTIL W-IDCATRAD = 15                                        
303900       IF SEGMENT-FINNS                                                   
303910*      -- Behöver inte ta bort underseg då logiska koppl ej finns         
303920*      -- Dock måste hänvisning till denna rad tas bort speciellt,        
303930*      -- eftersom seq-ix AVSG läggs upp av hänvisande rad.               
304000          MOVE RAD-KDCATPUB-TOM TO HANVISN-KDCATPUB-TOM                   
304200          MOVE W-WDN512KY-X  TO W-WDN5GSEQ-12-X                           
305100          PERFORM IMS-GU-AVSG-HAEN                                        
305200          PERFORM UNTIL SEGMENT-SAKNAS                                    
305300             PERFORM S99-FIXA-HANVISNING                                  
305310*            --- Tag bort hänvisnings-segm i hänvisnande avsnitt          
305320*            --- RAD-AVS3 läst med GU i S99-FIXA..                        
305400             PERFORM IMS-GHNP-AVS-HAEN-AVS3                               
305410             PERFORM IMS-DLET-AVS3                                        
305500             PERFORM IMS-GN-AVSG-HAEN                                     
305600          END-PERFORM                                                     
305700                                                                          
305800          PERFORM IMS-GHNP-AVS-RAD-FIRST                                  
305810*         --- Ta bort denna rad                                           
305900          PERFORM IMS-DLET-AVS                                            
306000       END-IF                                                             
306100       ADD +1 TO W-IDCATRAD                                               
306200       PERFORM IMS-GHNP-AVS-RAD                                           
306300     END-PERFORM                                                          
306301*    --- Tag även bort alla tillhörande VADIS-segment                     
306302     MOVE WS-KDCATPUB TO W-KDCATPUB-F13                                   
306310     PERFORM IMS-GHNP-AVS-VADIS                                           
306320     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
306321       PERFORM IMS-DLET-AVS                                               
306322       PERFORM IMS-GHNP-AVS-VADIS                                         
306330     END-PERFORM                                                          
306400                                                                          
306500     PERFORM S10-OMNUM-KDCATPUB-TOM                                       
306600     MOVE WS-KDCATPUB TO GALLANDE-KDCATPUB                                
306700     MOVE MED-1(SPRAAK-IX) TO MOD-TEMFSINF                                
306800     .                                                                    
306900     EJECT                                                                
307000 K-KOPIERING SECTION.                                                     
307100                                                                          
307200     IF MID-KDCATPUB-R-TOM = ALL '+'                                      
307300        MOVE HIGH-VALUE TO WS-KDCATPUB-TOM                                
307400     END-IF                                                               
307500                                                                          
307600     MOVE ZERO TO SPAR-IDILLU                                             
307700     MOVE NEJ TO GALLANDE-ILLU                                            
307800     PERFORM IMS-GU-AVS                                                   
307900     PERFORM IMS-GNP-AVS-ILLU                                             
308000     PERFORM UNTIL SEGMENT-SAKNAS OR (GALLANDE-ILLU = JA)                 
308100       IF ILLU-KDCATPUB-FOM  = WS-KDCATPUB                                
308200          MOVE ILLU-IDILLU TO SPAR-IDILLU                                 
308300          MOVE JA TO GALLANDE-ILLU                                        
308400       END-IF                                                             
308500       PERFORM IMS-GNP-AVS-ILLU                                           
308600     END-PERFORM                                                          
308700                                                                          
308800     IF GALLANDE-ILLU = JA                                                
308900        MOVE NEJ TO GALLANDE-ILLU                                         
309000        PERFORM IMS-GU-AVS                                                
309100        PERFORM IMS-GNP-AVS-ILLU                                          
309200        PERFORM UNTIL SEGMENT-SAKNAS OR (GALLANDE-ILLU = JA)              
309300          IF ILLU-KDCATPUB-FOM = WS-KDCATPUB-COPY                         
309400             MOVE JA TO GALLANDE-ILLU                                     
309500          END-IF                                                          
309600          PERFORM IMS-GNP-AVS-ILLU                                        
309700        END-PERFORM                                                       
309800                                                                          
309900        IF GALLANDE-ILLU = NEJ                                            
310000           MOVE SPAR-IDILLU      TO ILLU-IDILLU                           
310100           MOVE WS-KDCATPUB-COPY TO ILLU-KDCATPUB-FOM                     
310200           PERFORM IMS-ISRT-AVS-ILLU                                      
310300        END-IF                                                            
310400     END-IF                                                               
310500                                                                          
310501     PERFORM IMS-GHU-AVS                                                  
310502     IF SEGMENT-FINNS                                                     
310503*      --- Sätt det nya avsnittet till att vara ändrat                    
310504       MOVE JA TO AVS-FLAVSUST                                            
310505       PERFORM IMS-REPL-AVS                                               
310506     END-IF                                                               
310507                                                                          
310510     PERFORM IMS-GU-AVS                                                   
310600     MOVE +1 TO W-IDCATRAD                                                
310700     MOVE WS-KDCATPUB TO W-KDCATPUB                                       
310800     PERFORM IMS-GHNP-AVS-RAD                                             
310900     IF SEGMENT-FINNS                                                     
311000        MOVE WS-KDCATPUB-COPY  TO RAD-KDCATPUB-FOM                        
311100        MOVE WS-KDCATPUB-TOM   TO RAD-KDCATPUB-TOM                        
311200        MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                            
311300        MOVE 'N'               TO RAD-KDRADST                             
311400        MOVE MSG-SIGNON-USERID TO RAD-IDUSER                              
311500        MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                               
311510        MOVE 'FRÅN K-KOPIERING SECTION.' TO ABENDINFO                     
311600        PERFORM IMS-ISRT-AVS-RAD-PCB2                                     
311700                                                                          
311800        MOVE WS-KDCATPUB TO W-KDCATPUB                                    
311900        PERFORM IMS-GHNP-AVS-RUB                                          
312000        PERFORM UNTIL SEGMENT-SAKNAS                                      
312100           MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                            
312200           PERFORM IMS-ISRT-AVS-RUB-PCB2                                  
312300           MOVE WS-KDCATPUB TO W-KDCATPUB                                 
312400           PERFORM IMS-GHNP-AVS-RUB                                       
312500        END-PERFORM                                                       
312600     END-IF                                                               
312700                                                                          
312800     MOVE +4 TO W-IDCATRAD                                                
312900     MOVE WS-KDCATPUB TO W-KDCATPUB                                       
313000     PERFORM IMS-GHNP-AVS-RAD                                             
313100     IF SEGMENT-FINNS                                                     
313200        MOVE WS-KDCATPUB-COPY  TO RAD-KDCATPUB-FOM                        
313300        MOVE WS-KDCATPUB-TOM   TO RAD-KDCATPUB-TOM                        
313400        MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                            
313500        MOVE 'N'               TO RAD-KDRADST                             
313600        MOVE MSG-SIGNON-USERID TO RAD-IDUSER                              
313700        MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                               
313710        MOVE 'FRÅN K-KOPIERING SECTION.' TO ABENDINFO                     
313800        PERFORM IMS-ISRT-AVS-RAD-PCB2                                     
313900                                                                          
314000        MOVE WS-KDCATPUB TO W-KDCATPUB                                    
314100        PERFORM IMS-GHNP-AVS-TEXT                                         
314200        IF SEGMENT-FINNS                                                  
314300           MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                            
314400           PERFORM IMS-ISRT-AVS-TEXT-PCB2                                 
314500        END-IF                                                            
314600                                                                          
314700        MOVE WS-KDCATPUB TO W-KDCATPUB                                    
314800        PERFORM IMS-GHNP-AVS-FOT                                          
314900        PERFORM UNTIL SEGMENT-SAKNAS                                      
315000           MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                            
315100           PERFORM IMS-ISRT-AVS-FOT-PCB2                                  
315200           MOVE WS-KDCATPUB TO W-KDCATPUB                                 
315300           PERFORM IMS-GHNP-AVS-FOT                                       
315400        END-PERFORM                                                       
315500     END-IF                                                               
315600                                                                          
315700     MOVE +5 TO W-IDCATRAD                                                
315800     MOVE WS-KDCATPUB TO W-KDCATPUB                                       
315900     PERFORM IMS-GHNP-AVS-RAD                                             
316000     IF SEGMENT-FINNS                                                     
316100        MOVE WS-KDCATPUB-COPY  TO RAD-KDCATPUB-FOM                        
316200        MOVE WS-KDCATPUB-TOM   TO RAD-KDCATPUB-TOM                        
316300        MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                            
316400        MOVE 'N'               TO RAD-KDRADST                             
316500        MOVE MSG-SIGNON-USERID TO RAD-IDUSER                              
316600        MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                               
316610        MOVE 'FRÅN K-KOPIERING SECTION.' TO ABENDINFO                     
316700        PERFORM IMS-ISRT-AVS-RAD-PCB2                                     
316800                                                                          
316900        MOVE WS-KDCATPUB TO W-KDCATPUB                                    
317000        PERFORM IMS-GHNP-AVS-TEXT                                         
317100        IF SEGMENT-FINNS                                                  
317200           MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                            
317300           PERFORM IMS-ISRT-AVS-TEXT-PCB2                                 
317400        END-IF                                                            
317500     END-IF                                                               
317600                                                                          
317700     MOVE +10 TO W-IDCATRAD                                               
317800     MOVE WS-KDCATPUB TO W-KDCATPUB                                       
317900     PERFORM IMS-GHNP-AVS-RAD                                             
318000     PERFORM UNTIL W-IDCATRAD = 15                                        
318100       IF SEGMENT-FINNS                                                   
318200          MOVE WS-KDCATPUB-COPY  TO RAD-KDCATPUB-FOM                      
318300          MOVE WS-KDCATPUB-TOM   TO RAD-KDCATPUB-TOM                      
318400          MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                          
318500          MOVE 'N'               TO RAD-KDRADST                           
318600          MOVE MSG-SIGNON-USERID TO RAD-IDUSER                            
318700          MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                             
318710          MOVE 'FRÅN K-KOPIERING SECTION.' TO ABENDINFO                   
318800          PERFORM IMS-ISRT-AVS-RAD-PCB2                                   
318900                                                                          
319000          MOVE WS-KDCATPUB TO W-KDCATPUB                                  
319100          PERFORM IMS-GHNP-AVS-TEXT                                       
319200          IF SEGMENT-FINNS                                                
319300             MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                          
319400             PERFORM IMS-ISRT-AVS-TEXT-PCB2                               
319500          END-IF                                                          
319600                                                                          
319700          MOVE WS-KDCATPUB TO W-KDCATPUB                                  
319800          PERFORM IMS-GHNP-AVS-FOT                                        
319900          PERFORM UNTIL SEGMENT-SAKNAS                                    
320000             MOVE WS-KDCATPUB-COPY TO W-KDCATPUB                          
320100             PERFORM IMS-ISRT-AVS-FOT-PCB2                                
320200             MOVE WS-KDCATPUB TO W-KDCATPUB                               
320300             PERFORM IMS-GHNP-AVS-FOT                                     
320400          END-PERFORM                                                     
320500       END-IF                                                             
320600       ADD +1 TO W-IDCATRAD                                               
320700       MOVE WS-KDCATPUB TO W-KDCATPUB                                     
320800       PERFORM IMS-GHNP-AVS-RAD                                           
320900     END-PERFORM                                                          
320901*    --- Kopiera även VADIS segmenten                                     
320902     MOVE WS-KDCATPUB TO W-KDCATPUB-F13                                   
320903     PERFORM IMS-GHNP-AVS-VADIS                                           
320904     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
320905       IF SEGMENT-FINNS                                                   
320911          MOVE WS-KDCATPUB-COPY TO VADIS-KDCATPUB-FOM                     
320920          MOVE WS-KDCATPUB-TOM  TO VADIS-KDCATPUB-TOM                     
320922                                                                          
320923          PERFORM IMS-ISRT-AVS-VADIS-PCB2                                 
320925          PERFORM IMS-GHNP-AVS-VADIS                                      
320926       END-IF                                                             
320930     END-PERFORM                                                          
321000                                                                          
321100     PERFORM S10-OMNUM-KDCATPUB-TOM                                       
321200     PERFORM S11-OMNUM-KDCATPUB-FROM                                      
321300     MOVE MED-9(SPRAAK-IX) TO MOD-TEMFSINF                                
321400     MOVE MED-7(SPRAAK-IX) TO MOD-TEMFSFEL                                
321500     MOVE WS-KDCATPUB-COPY TO GALLANDE-KDCATPUB                           
321600     .                                                                    
321700     EJECT                                                                
321800 L-UPPDAT-KDCATPUB-FROM SECTION.                                          
321900                                                                          
322000     MOVE SW-OMRAKN-FROM TO SPAR-OMRAKN-FROM                              
322100     MOVE KDCATPUB-OLD TO SPAR-KDCATPUB-OLD                               
322200     MOVE KDCATPUB-NEW TO SPAR-KDCATPUB-NEW                               
322300     MOVE WS-KDCATPUB TO KDCATPUB-OLD                                     
322400     MOVE WS-KDCATPUB-FROM TO KDCATPUB-NEW                                
322500     MOVE JA TO SW-OMRAKN-FROM                                            
322600     PERFORM S11-OMNUM-KDCATPUB-FROM                                      
322700                                                                          
322800     MOVE SPAR-OMRAKN-FROM TO SW-OMRAKN-FROM                              
322900     MOVE SPAR-KDCATPUB-NEW TO KDCATPUB-NEW                               
323000     MOVE SPAR-KDCATPUB-OLD TO KDCATPUB-OLD                               
323100     PERFORM S10-OMNUM-KDCATPUB-TOM                                       
323200                                                                          
323300     MOVE WS-KDCATPUB-FROM TO GALLANDE-KDCATPUB                           
323400     MOVE MED-9(SPRAAK-IX) TO MOD-TEMFSINF                                
323500     .                                                                    
323600     EJECT                                                                
323700 X-GRUND-FORMAT SECTION.                                                  
323800     SKIP2                                                                
323900     MOVE +1 TO INDX                                                      
324000     PERFORM UNTIL INDX >= +6                                             
324100       MOVE MFS-FORMATETS-ATTR TO MOD-IDRUBNR-ATTR(INDX)                  
324200                                  MOD-TEKOL-ATTR(INDX)                    
324300                                  MOD-IDFOTNR-ATTR(INDX, 1)               
324400                                  MOD-IDFOTNR-ATTR(INDX, 2)               
324500                                  MOD-IDFOTNR-ATTR(INDX, 3)               
324600       ADD +1 TO INDX                                                     
324700     END-PERFORM                                                          
324800                                                                          
324900     MOVE +1 TO INDX                                                      
325000     PERFORM UNTIL INDX >= +4                                             
325100       MOVE MFS-FORMATETS-ATTR TO MOD-IDFOTNR-RAD4-ATTR(INDX)             
325200       ADD +1 TO INDX                                                     
325300     END-PERFORM                                                          
325400     MOVE MFS-FORMATETS-ATTR TO MOD-BERUBTEXT-1-ATTR                      
325500                                MOD-BERUBTEXT-2-ATTR                      
325600                                MOD-BERUBTEXT-3-ATTR                      
325700                                MOD-BERUBTEXT-4-ATTR                      
325800                                MOD-IDILLU-ATTR                           
325900                                MOD-BORT-ATTR                             
326000                                MOD-KDCATPUB-R-COPY-ATTR                  
326100                                MOD-KDCATPUB-R-FROM-ATTR                  
326200                                MOD-KDCATPUB-R-TOM-ATTR                   
326300     .                                                                    
326400     EJECT                                                                
326500 Y-KOLLA-PF-TANGENTER SECTION.                                            
326600                                                                          
326700     EVALUATE TRUE                                                        
326800        WHEN MFS-NEXT                                                     
326900           IF MID-KDCATPUB-R-NEXT NUMERIC                                 
327000              MOVE WS-KDCATPUB-NEXT TO GALLANDE-KDCATPUB                  
327100           ELSE                                                           
327200              MOVE LOW-VALUE TO W-KDCATPUB                                
327300              PERFORM YB-SOEK-FORSTA                                      
327400           END-IF                                                         
327500        WHEN MFS-FIRST                                                    
327600           MOVE LOW-VALUE TO W-KDCATPUB                                   
327700           PERFORM YB-SOEK-FORSTA                                         
327800        WHEN MFS-ENTER                                                    
327900           MOVE WS-KDCATPUB TO W-KDCATPUB                                 
328000           PERFORM YA-SOEK-GALLANDE                                       
328100        WHEN OTHER                                                        
328200           MOVE LOW-VALUE TO W-KDCATPUB                                   
328300           PERFORM YB-SOEK-FORSTA                                         
328400     END-EVALUATE                                                         
328500     .                                                                    
328600     EJECT                                                                
328700 YA-SOEK-GALLANDE SECTION.                                                
328800                                                                          
328900     MOVE NEJ TO GALLANDE-FINNS                                           
329000                 AVS-UTGANGSMARKERAT                                      
329100                 RADER-FINNS                                              
329200     MOVE LOW-VALUE TO GALLANDE-KDCATPUB                                  
329300                       SPAR-KDCATPUB                                      
329400     PERFORM IMS-GU-AVS                                                   
329500     IF SEGMENT-FINNS                                                     
329600        MOVE JA TO RADER-FINNS                                            
329700        MOVE +1 TO W-IDCATRAD                                             
329800        PERFORM IMS-GNP-AVS-RAD                                           
329900        PERFORM UNTIL W-IDCATRAD > 14 OR (GALLANDE-FINNS = JA)            
330000           IF SEGMENT-FINNS                                               
330100              MOVE W-KDCATPUB TO GALLANDE-KDCATPUB                        
330200              MOVE JA TO GALLANDE-FINNS                                   
330300           END-IF                                                         
330400           ADD +1 TO W-IDCATRAD                                           
330500           PERFORM IMS-GNP-AVS-RAD                                        
330600        END-PERFORM                                                       
330700                                                                          
330800        IF GALLANDE-FINNS = NEJ                                           
330900           MOVE NEJ TO RADER-FINNS                                        
331000           MOVE LOW-VALUE TO W-KDCATPUB-MIN                               
331100           MOVE HIGH-VALUE TO W-KDCATPUB-MAX                              
331200           MOVE +1 TO W-IDCATRAD-MIN                                      
331300           MOVE +14 TO W-IDCATRAD-MAX                                     
331400                                                                          
331500           PERFORM IMS-GNP-AVS-RAD-SOEK-FIRST                             
331600           PERFORM UNTIL SEGMENT-SAKNAS                                   
331700              MOVE JA TO RADER-FINNS                                      
331800              IF WS-KDCATPUB = LOW-VALUE                                  
331900                 IF RAD-KDCATPUB-FOM <= DAGENS-VECKAS-KATPUB              
332000                    IF RAD-KDCATPUB-FOM > SPAR-KDCATPUB                   
332100                       IF RAD-KDCATPUB-TOM >= DAGENS-VECKAS-KATPUB        
332200                          MOVE RAD-KDCATPUB-FOM                           
332300                                          TO GALLANDE-KDCATPUB            
332400                                             SPAR-KDCATPUB                
332500                          MOVE JA TO GALLANDE-FINNS                       
332600                       END-IF                                             
332700                    END-IF                                                
332800                 END-IF                                                   
332900              ELSE                                                        
333000**********************  NYCKEL IFYLLD                                     
333100                 IF RAD-KDCATPUB-FOM <= WS-KDCATPUB                       
333200                    IF RAD-KDCATPUB-FOM > SPAR-KDCATPUB                   
333300                      IF RAD-KDCATPUB-TOM >= WS-KDCATPUB                  
333400                         MOVE RAD-KDCATPUB-FOM TO                         
333500                                            GALLANDE-KDCATPUB             
333600                                            SPAR-KDCATPUB                 
333700                         MOVE JA TO GALLANDE-FINNS                        
333800                      END-IF                                              
333900                    END-IF                                                
334000                 END-IF                                                   
334100              END-IF                                                      
334200              PERFORM IMS-GNP-AVS-RAD-SOEK                                
334300          END-PERFORM                                                     
334400        END-IF                                                            
334500     END-IF                                                               
334600     IF RADER-FINNS = JA                                                  
334700        IF GALLANDE-FINNS = NEJ                                           
334800           MOVE JA TO AVS-UTGANGSMARKERAT                                 
334900        END-IF                                                            
335000     ELSE                                                                 
335100        MOVE WS-KDCATPUB TO GALLANDE-KDCATPUB                             
335200     END-IF                                                               
335300     .                                                                    
335400     EJECT                                                                
335500 YB-SOEK-FORSTA SECTION.                                                  
335600                                                                          
335700     MOVE NEJ TO GALLANDE-FINNS                                           
335800     MOVE SPACE TO GALLANDE-KDCATPUB                                      
335900     MOVE HIGH-VALUE TO LAS-KDCATPUB                                      
336000     MOVE LOW-VALUE TO W-KDCATPUB-MIN                                     
336100     MOVE HIGH-VALUE TO W-KDCATPUB-MAX                                    
336200     MOVE +1 TO W-IDCATRAD-MIN                                            
336300                W-IDCATRAD-MAX                                            
336400                                                                          
336500     PERFORM IMS-GU-AVS                                                   
336600     IF SEGMENT-FINNS                                                     
336700        PERFORM IMS-GNP-AVS-RAD-SOEK-FIRST                                
336800        PERFORM UNTIL W-IDCATRAD-MIN > 14                                 
336900           IF SEGMENT-FINNS                                               
337000              IF RAD-KDCATPUB-FOM < LAS-KDCATPUB                          
337100                 MOVE JA TO GALLANDE-FINNS                                
337200                 MOVE RAD-KDCATPUB-FOM TO GALLANDE-KDCATPUB               
337300                                          LAS-KDCATPUB                    
337400              END-IF                                                      
337500           END-IF                                                         
337600           ADD +1 TO W-IDCATRAD-MIN                                       
337700                     W-IDCATRAD-MAX                                       
337800           PERFORM IMS-GNP-AVS-RAD-SOEK                                   
337900        END-PERFORM                                                       
338000     END-IF                                                               
338100     .                                                                    
338200     EJECT                                                                
338300 S01-GET-AVS-RAD SECTION.                                                 
338400     SKIP2                                                                
338500     PERFORM IMS-GHNP-AVS-RAD                                             
338600     IF SEGMENT-SAKNAS                                                    
338700       MOVE W-IDCATRAD        TO RAD-IDCATRAD                             
338800       MOVE WS-KDCATPUB       TO RAD-KDCATPUB-FOM                         
338900******** NY                                                               
339000       IF SW-UPPDATERING = JA AND MID-KDCATPUB-R-TOM = ALL '+'            
339100          MOVE FIXAD-KDCATPUB-TOM TO RAD-KDCATPUB-TOM                     
339200       ELSE                                                               
339300          MOVE WS-KDCATPUB-TOM   TO RAD-KDCATPUB-TOM                      
339400       END-IF                                                             
339500******** NY                                                               
339600       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
339700       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
339800       MOVE 'N'               TO RAD-KDRADST                              
339810       MOVE 'FRÅN  S01-GET-AVS-RAD SECTION.            '                  
339820                         TO ABENDINFO                                     
339900       PERFORM IMS-ISRT-AVS-RAD                                           
340000     ELSE                                                                 
340100       MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                             
340200       MOVE 'Ä'               TO RAD-KDRADST                              
340300       MOVE MSG-SIGNON-USERID TO RAD-IDUSER                               
340400       PERFORM IMS-REPL-AVS                                               
340500     END-IF                                                               
340600     .                                                                    
340700     EJECT                                                                
340800 S02-KOLLA-GILTIG-KATPUB SECTION.                                         
340900                                                                          
341000     MOVE NEJ TO GODK-KATPUB-FINNS                                        
341100     IF TEST-KDCATPUB-FROM = LOW-VALUE                                    
341200        MOVE JA TO GODK-KATPUB-FINNS                                      
341300     ELSE                                                                 
341400        MOVE W-IDCATNR TO W-IDCATNR-WDN1                                  
341500        PERFORM IMS-GU-KAT                                                
341600        IF SEGMENT-FINNS                                                  
341700           PERFORM IMS-GNP-KATM11                                         
341800           PERFORM UNTIL SEGMENT-SAKNAS                                   
341900              MOVE +1 TO PER-IX                                           
342000              PERFORM UNTIL PER-IX > PER-IX-MAX                           
342100                 IF TAB-KDCATPUB-FOM(PER-IX) =                            
342200                    TEST-KDCATPUB-FROM                                    
342300                        MOVE JA TO GODK-KATPUB-FINNS                      
342400                 END-IF                                                   
342500                 ADD +1 TO PER-IX                                         
342600              END-PERFORM                                                 
342700              PERFORM IMS-GNP-KATM11                                      
342800           END-PERFORM                                                    
342900         END-IF                                                           
343000     END-IF                                                               
343100                                                                          
343200     IF GODK-KATPUB-FINNS = NEJ                                           
343300        IF SW-KOPIERING = JA                                              
343400            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR           
343500            MOVE JA TO INDATA-FEL                                         
343600        ELSE                                                              
343700            IF MID-KDCATPUB-R-FROM NOT = ALL '+'                          
343800               MOVE MFS-ALFA-FAELT-FEL                                    
343900                             TO MOD-KDCATPUB-R-FROM-ATTR                  
344000            ELSE                                                          
344100               MOVE FEL-3(SPRAAK-IX) TO MOD-TEMFSINF                      
344200            END-IF                                                        
344300            MOVE JA TO INDATA-FEL                                         
344400        END-IF                                                            
344500     END-IF                                                               
344600     .                                                                    
344700     EJECT                                                                
344800 S03-KOLLA-TABELL SECTION.                                                
344900                                                                          
345000     MOVE NEJ TO SW-OMRAKN-FROM                                           
345100                 SW-OMRAKN-TOM                                            
345200     MOVE ZERO TO TEST-IX                                                 
345300     MOVE +198 TO TAB-IX-MAX                                              
345400                                                                          
345500     IF SW-UPPDATERING = JA                                               
345600        IF MID-KDCATPUB-R-TOM = ALL '+'                                   
345700           CONTINUE                                                       
345800        ELSE                                                              
345900           MOVE WS-KDCATPUB-TOM TO TEST-KDCATPUB-TOM                      
346000           MOVE +1 TO TAB-IX                                              
346100           PERFORM UNTIL TAB-IX > TAB-IX-MAX                              
346200              IF TAB2-KDCATPUB-TOM(TAB-IX) = TEST-KDCATPUB-TOM            
346300               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR        
346400                 MOVE JA TO INDATA-FEL                                    
346500                 MOVE +200 TO TAB-IX-MAX                                  
346600              END-IF                                                      
346700              ADD +2 TO TAB-IX                                            
346800           END-PERFORM                                                    
346900                                                                          
347000           MOVE +1 TO TAB-IX                                              
347100           PERFORM UNTIL TAB-IX > TAB-IX-MAX                              
347200              IF WS-KDCATPUB = TAB2-KDCATPUB-FROM(TAB-IX)                 
347300                 MOVE TEST-KDCATPUB-TOM                                   
347400                                TO TAB2-KDCATPUB-TOM(TAB-IX)              
347500                 MOVE TAB2-KDCATPUB-FROM(TAB-IX)                          
347600                                TO TEST-KDCATPUB-FROM                     
347700                 MOVE TAB-IX TO TEST-IX                                   
347800                 MOVE +200 TO TAB-IX                                      
347900              ELSE                                                        
348000                 ADD +1 TO TAB-IX                                         
348100              END-IF                                                      
348200           END-PERFORM                                                    
348300        END-IF                                                            
348400     END-IF                                                               
348500                                                                          
348600     IF SW-BORTTAG = JA                                                   
348700        MOVE +1 TO TAB-IX                                                 
348800        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
348900           IF WS-KDCATPUB = TAB2-KDCATPUB-FROM(TAB-IX)                    
349000              MOVE TAB-IX TO TEST-IX                                      
349100              MOVE +200 TO TAB-IX                                         
349200           ELSE                                                           
349300              ADD +1 TO TAB-IX                                            
349400           END-IF                                                         
349500        END-PERFORM                                                       
349600     END-IF                                                               
349700                                                                          
349800     IF SW-KDCATPUB-FROM = JA                                             
349900        MOVE WS-KDCATPUB-FROM TO TEST-KDCATPUB-FROM                       
350000        MOVE +2 TO TAB-IX                                                 
350100        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
350200           IF TAB2-KDCATPUB-FROM(TAB-IX) = TEST-KDCATPUB-FROM             
350300              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR         
350400              MOVE JA TO INDATA-FEL                                       
350500              MOVE +200 TO TAB-IX-MAX                                     
350600           END-IF                                                         
350700           ADD +2 TO TAB-IX                                               
350800        END-PERFORM                                                       
350900                                                                          
351000        MOVE +2 TO TAB-IX                                                 
351100        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
351200           IF WS-KDCATPUB  = TAB2-KDCATPUB-FROM(TAB-IX)                   
351300              MOVE TEST-KDCATPUB-FROM                                     
351400                             TO TAB2-KDCATPUB-FROM(TAB-IX)                
351500              MOVE TAB-IX TO TEST-IX                                      
351600              MOVE TAB2-KDCATPUB-TOM(TAB-IX)                              
351700                             TO TEST-KDCATPUB-TOM                         
351800           END-IF                                                         
351900           ADD +2 TO TAB-IX                                               
352000        END-PERFORM                                                       
352100     END-IF                                                               
352200                                                                          
352300     IF SW-KOPIERING = JA                                                 
352400        MOVE WS-KDCATPUB-COPY TO TEST-KDCATPUB-FROM                       
352500        IF MID-KDCATPUB-R-TOM = ALL '+'                                   
352600           MOVE HIGH-VALUE TO TEST-KDCATPUB-TOM                           
352700        ELSE                                                              
352800           MOVE MID-KDCATPUB-R-TOM TO WS-KDCATPUB-R-AVV                   
352820           PERFORM S50-Y2K-KDCATPUB-R                                     
352830           MOVE WS-KDCATPUB-AAAAVV                                        
352840                                 TO TEST-KDCATPUB-TOM                     
352900        END-IF                                                            
353000                                                                          
353100        MOVE +2 TO TAB-IX                                                 
353200        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
353300           IF TAB2-KDCATPUB-FROM(TAB-IX) = TEST-KDCATPUB-FROM             
353400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR         
353500              MOVE JA TO INDATA-FEL                                       
353600              MOVE +200 TO TAB-IX-MAX                                     
353700           END-IF                                                         
353800           ADD +2 TO TAB-IX                                               
353900        END-PERFORM                                                       
354000        MOVE +2 TO TAB-IX                                                 
354100        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
354200           IF TAB2-KDCATPUB-TOM(TAB-IX) = TEST-KDCATPUB-TOM               
354300              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR         
354400              MOVE JA TO INDATA-FEL                                       
354500              MOVE +200 TO TAB-IX-MAX                                     
354600           END-IF                                                         
354700           ADD +2 TO TAB-IX                                               
354800        END-PERFORM                                                       
354900                                                                          
355000        MOVE +2 TO TAB-IX                                                 
355100        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
355200           IF TEST-KDCATPUB-FROM > TAB2-KDCATPUB-FROM(TAB-IX)             
355300              IF TAB2-KDCATPUB-FROM(TAB-IX) = SPACE                       
355400                 ADD -1 TO TAB-IX                                         
355500                 MOVE TEST-KDCATPUB-FROM                                  
355600                                TO TAB2-KDCATPUB-FROM(TAB-IX)             
355700                 MOVE TEST-KDCATPUB-TOM                                   
355800                                TO TAB2-KDCATPUB-TOM(TAB-IX)              
355900                 MOVE TAB-IX TO TEST-IX                                   
356000                 MOVE +200 TO TAB-IX                                      
356100              ELSE                                                        
356200                 ADD +2 TO TAB-IX                                         
356300                 IF TEST-KDCATPUB-FROM > TAB2-KDCATPUB-FROM               
356400                                    (TAB-IX)                              
356500                    ADD -2 TO TAB-IX                                      
356600                 ELSE                                                     
356700                   ADD -2 TO TAB-IX                                       
356800                   ADD +1 TO TAB-IX                                       
356900                   MOVE TEST-KDCATPUB-FROM                                
357000                                  TO TAB2-KDCATPUB-FROM(TAB-IX)           
357100                   MOVE TEST-KDCATPUB-TOM                                 
357200                                  TO TAB2-KDCATPUB-TOM(TAB-IX)            
357300                   MOVE TAB-IX TO TEST-IX                                 
357400                   MOVE +200 TO TAB-IX                                    
357500                 END-IF                                                   
357600              END-IF                                                      
357700           ELSE                                                           
357800              ADD -1 TO TAB-IX                                            
357900              MOVE TEST-KDCATPUB-FROM                                     
358000                             TO TAB2-KDCATPUB-FROM(TAB-IX)                
358100              MOVE TEST-KDCATPUB-TOM                                      
358200                             TO TAB2-KDCATPUB-TOM(TAB-IX)                 
358300              MOVE TAB-IX TO TEST-IX                                      
358400              MOVE +200 TO TAB-IX                                         
358500           END-IF                                                         
358600           ADD +2 TO TAB-IX                                               
358700        END-PERFORM                                                       
358800     END-IF                                                               
358900                                                                          
359000     IF SW-NYUPPLAGG = JA                                                 
359100        MOVE WS-KDCATPUB TO TEST-KDCATPUB-FROM                            
359200        IF MID-KDCATPUB-R-TOM = ALL '+'                                   
359300           MOVE HIGH-VALUE TO TEST-KDCATPUB-TOM                           
359400        ELSE                                                              
359510           MOVE MID-KDCATPUB-R-TOM TO WS-KDCATPUB-R-AVV                   
359520           PERFORM S50-Y2K-KDCATPUB-R                                     
359530           MOVE WS-KDCATPUB-AAAAVV                                        
359540                                 TO TEST-KDCATPUB-TOM                     
359600        END-IF                                                            
359700                                                                          
359800        MOVE +2 TO TAB-IX                                                 
359900        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
360000           IF TAB2-KDCATPUB-FROM(TAB-IX) = TEST-KDCATPUB-FROM             
360100              MOVE JA TO NYCKLAR-FEL                                      
360200              MOVE +200 TO TAB-IX-MAX                                     
360300           END-IF                                                         
360400           ADD +2 TO TAB-IX                                               
360500        END-PERFORM                                                       
360600        MOVE +2 TO TAB-IX                                                 
360700        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
360800           IF TAB2-KDCATPUB-TOM(TAB-IX) = TEST-KDCATPUB-TOM               
360900              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR          
361000              MOVE JA TO INDATA-FEL                                       
361100              MOVE +200 TO TAB-IX-MAX                                     
361200           END-IF                                                         
361300           ADD +2 TO TAB-IX                                               
361400        END-PERFORM                                                       
361500                                                                          
361600        MOVE +2 TO TAB-IX                                                 
361700        PERFORM UNTIL TAB-IX > TAB-IX-MAX                                 
361800           IF TEST-KDCATPUB-FROM > TAB2-KDCATPUB-FROM(TAB-IX)             
361900              IF TAB2-KDCATPUB-FROM(TAB-IX) = SPACE                       
362000                 ADD -1 TO TAB-IX                                         
362100                 MOVE TEST-KDCATPUB-FROM                                  
362200                                TO TAB2-KDCATPUB-FROM(TAB-IX)             
362300                 MOVE TEST-KDCATPUB-TOM                                   
362400                                TO TAB2-KDCATPUB-TOM(TAB-IX)              
362500                 MOVE TAB-IX TO TEST-IX                                   
362600                 MOVE +200 TO TAB-IX                                      
362700              ELSE                                                        
362800                 ADD +2 TO TAB-IX                                         
362900                 IF TEST-KDCATPUB-FROM > TAB2-KDCATPUB-FROM               
363000                                    (TAB-IX)                              
363100                    ADD -2 TO TAB-IX                                      
363200                 ELSE                                                     
363300                   ADD -2 TO TAB-IX                                       
363400                   ADD +1 TO TAB-IX                                       
363500                   MOVE TEST-KDCATPUB-FROM                                
363600                                  TO TAB2-KDCATPUB-FROM(TAB-IX)           
363700                   MOVE TEST-KDCATPUB-TOM                                 
363800                                  TO TAB2-KDCATPUB-TOM(TAB-IX)            
363900                   MOVE TAB-IX TO TEST-IX                                 
364000                   MOVE +200 TO TAB-IX                                    
364100                 END-IF                                                   
364200              END-IF                                                      
364300           ELSE                                                           
364400              ADD -1 TO TAB-IX                                            
364500              MOVE TEST-KDCATPUB-FROM                                     
364600                             TO TAB2-KDCATPUB-FROM(TAB-IX)                
364700              MOVE TEST-KDCATPUB-TOM                                      
364800                             TO TAB2-KDCATPUB-TOM(TAB-IX)                 
364900              MOVE TAB-IX TO TEST-IX                                      
365000              MOVE +200 TO TAB-IX                                         
365100           END-IF                                                         
365200           ADD +2 TO TAB-IX                                               
365300        END-PERFORM                                                       
365400     END-IF                                                               
365500                                                                          
365600******* KOLLA TABELLERNA                                                  
365700                                                                          
365800     IF SW-KOPIERING = JA                                                 
365900        MOVE TEST-IX TO TAB-IX                                            
366000        IF TEST-IX = 1                                                    
366100           CONTINUE                                                       
366200        ELSE                                                              
366300           ADD -1 TO TAB-IX                                               
366400*          IF TAB2-KDCATPUB-TOM(TAB-IX) >= TEST-KDCATPUB-FROM             
366500              ADD -1 TO KDCATPUB-FROM-TEST                                
366600              MOVE TEST-KDCATPUB-FROM TO TAB2-KDCATPUB-TOM(TAB-IX)        
366700              ADD +1 TO KDCATPUB-FROM-TEST                                
366800              IF TAB2-KDCATPUB-TOM(TAB-IX)                                
366900                                >= TAB2-KDCATPUB-FROM(TAB-IX)             
367000                 MOVE TAB-IX TO UPPDAT-TOM-IX                             
367100                 MOVE JA TO SW-OMRAKN-TOM                                 
367200              ELSE                                                        
367300               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR        
367400                MOVE JA TO INDATA-FEL                                     
367500             END-IF                                                       
367600*          ELSE                                                           
367700*            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR          
367800*            MOVE JA TO INDATA-FEL                                        
367900*         END-IF                                                          
368000       END-IF                                                             
368100                                                                          
368200       MOVE TEST-IX TO TAB-IX                                             
368300       ADD +1 TO TAB-IX                                                   
368400        IF TAB2-KDCATPUB-FROM(TAB-IX) NOT = SPACE                         
368500          IF TEST-KDCATPUB-TOM = HIGH-VALUE                               
368600              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR         
368700              MOVE JA TO INDATA-FEL                                       
368800          ELSE                                                            
368900*            IF TAB2-KDCATPUB-FROM(TAB-IX) >= TEST-KDCATPUB-TOM           
369000              ADD +1 TO KDCATPUB-TOM-TEST                                 
369100              MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO KDCATPUB-OLD             
369200              MOVE TEST-KDCATPUB-TOM TO TAB2-KDCATPUB-FROM(TAB-IX)        
369300                                                                          
369400              ADD -1 TO KDCATPUB-TOM-TEST                                 
369500              IF TAB2-KDCATPUB-TOM(TAB-IX)                                
369600                                 >= TAB2-KDCATPUB-FROM(TAB-IX)            
369700                 MOVE JA TO SW-OMRAKN-FROM                                
369800                 MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO KDCATPUB-NEW          
369900                 MOVE TAB-IX TO UPPDAT-FROM-IX                            
370000              ELSE                                                        
370100               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR        
370200                 MOVE JA TO INDATA-FEL                                    
370300              END-IF                                                      
370400*           ELSE                                                          
370500*             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR         
370600*             MOVE JA TO INDATA-FEL                                       
370700*           END-IF                                                        
370800           END-IF                                                         
370900        END-IF                                                            
371000     END-IF                                                               
371100                                                                          
371200     IF SW-NYUPPLAGG = JA                                                 
371300        MOVE TEST-IX TO TAB-IX                                            
371400        IF TEST-IX = 1                                                    
371500           CONTINUE                                                       
371600        ELSE                                                              
371700           ADD -1 TO TAB-IX                                               
371800*          IF TAB2-KDCATPUB-TOM(TAB-IX) >= TEST-KDCATPUB-FROM             
371900              ADD -1 TO KDCATPUB-FROM-TEST                                
372000              MOVE TEST-KDCATPUB-FROM TO TAB2-KDCATPUB-TOM(TAB-IX)        
372100              ADD +1 TO KDCATPUB-FROM-TEST                                
372200              IF TAB2-KDCATPUB-TOM(TAB-IX)                                
372300                                >= TAB2-KDCATPUB-FROM(TAB-IX)             
372400                 MOVE TAB-IX TO UPPDAT-TOM-IX                             
372500                 MOVE JA TO SW-OMRAKN-TOM                                 
372600              ELSE                                                        
372700               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR         
372800                 MOVE JA TO INDATA-FEL                                    
372900             END-IF                                                       
373000*          ELSE                                                           
373100*            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR           
373200*            MOVE JA TO INDATA-FEL                                        
373300*         END-IF                                                          
373400       END-IF                                                             
373500                                                                          
373600       MOVE TEST-IX TO TAB-IX                                             
373700       ADD +1 TO TAB-IX                                                   
373800        IF TAB2-KDCATPUB-FROM(TAB-IX) NOT = SPACE                         
373900          IF TEST-KDCATPUB-TOM = HIGH-VALUE                               
374000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-tom-ATTR          
374100              MOVE JA TO INDATA-FEL                                       
374200          ELSE                                                            
374300*            IF TAB2-KDCATPUB-FROM(TAB-IX) >= TEST-KDCATPUB-TOM           
374400              ADD +1 TO KDCATPUB-TOM-TEST                                 
374500              MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO KDCATPUB-OLD             
374600              MOVE TEST-KDCATPUB-TOM TO TAB2-KDCATPUB-FROM(TAB-IX)        
374700                                                                          
374800              ADD -1 TO KDCATPUB-TOM-TEST                                 
374900              IF TAB2-KDCATPUB-TOM(TAB-IX)                                
375000                                >= TAB2-KDCATPUB-FROM(TAB-IX)             
375100                 MOVE JA TO SW-OMRAKN-FROM                                
375200                 MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO KDCATPUB-NEW          
375300                 MOVE TAB-IX TO UPPDAT-FROM-IX                            
375400              ELSE                                                        
375500               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR         
375600                 MOVE JA TO INDATA-FEL                                    
375700              END-IF                                                      
375800*           ELSE                                                          
375900*             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR          
376000*             MOVE JA TO INDATA-FEL                                       
376100*           END-IF                                                        
376200        END-IF                                                            
376300        END-IF                                                            
376400     END-IF                                                               
376500                                                                          
376600     IF SW-BORTTAG = JA                                                   
376700        IF TEST-IX = 2                                                    
376800           CONTINUE                                                       
376900        ELSE                                                              
377000          MOVE TEST-IX TO TAB-IX                                          
377100          ADD +2 TO TAB-IX                                                
377200          IF TAB2-KDCATPUB-FROM(TAB-IX) NOT = SPACE                       
377300            MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO TEST-KDCATPUB-FROM         
377400            MOVE TEST-IX TO TAB-IX                                        
377500            ADD -2 TO TAB-IX                                              
377600            ADD -1 TO KDCATPUB-FROM-TEST                                  
377700            MOVE TEST-KDCATPUB-FROM TO TAB2-KDCATPUB-TOM(TAB-IX)          
377800            IF TAB2-KDCATPUB-TOM(TAB-IX)                                  
377900                                 >= TAB2-KDCATPUB-FROM(TAB-IX)            
378000               MOVE JA TO SW-OMRAKN-TOM                                   
378100               MOVE TAB-IX TO UPPDAT-TOM-IX                               
378200            ELSE                                                          
378300               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR        
378400               MOVE JA TO INDATA-FEL                                      
378500            END-IF                                                        
378600          END-IF                                                          
378700        END-IF                                                            
378800     END-IF                                                               
378900                                                                          
379000     IF SW-KDCATPUB-FROM = JA                                             
379100        IF TEST-IX = 2                                                    
379200           CONTINUE                                                       
379300        ELSE                                                              
379400           MOVE TEST-IX TO TAB-IX                                         
379500           ADD -2 TO TAB-IX                                               
379600           ADD -1 TO KDCATPUB-FROM-TEST                                   
379700           MOVE TEST-KDCATPUB-FROM TO TAB2-KDCATPUB-TOM(TAB-IX)           
379800           ADD +1 TO KDCATPUB-FROM-TEST                                   
379900           IF TAB2-KDCATPUB-TOM(TAB-IX)                                   
380000                             >= TAB2-KDCATPUB-FROM(TAB-IX)                
380100              MOVE TAB-IX TO UPPDAT-TOM-IX                                
380200              MOVE JA TO SW-OMRAKN-TOM                                    
380300           ELSE                                                           
380400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-COPY-ATTR         
380500              MOVE JA TO INDATA-FEL                                       
380600           END-IF                                                         
380700        END-IF                                                            
380800     END-IF                                                               
380900                                                                          
381000     IF SW-UPPDATERING = JA                                               
381100      IF MID-KDCATPUB-R-TOM = ALL '+'                                     
381200         CONTINUE                                                         
381300      ELSE                                                                
381400         MOVE TEST-IX TO TAB-IX                                           
381500         ADD +2 TO TAB-IX                                                 
381600         IF TAB2-KDCATPUB-FROM(TAB-IX) NOT = SPACE                        
381700            ADD +1 TO KDCATPUB-TOM-TEST                                   
381800            MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO KDCATPUB-OLD               
381900            MOVE TEST-KDCATPUB-TOM TO TAB2-KDCATPUB-FROM                  
382000                                    (TAB-IX)                              
382100            ADD -1 TO KDCATPUB-TOM-TEST                                   
382200            IF TAB2-KDCATPUB-TOM(TAB-IX)                                  
382300                               >= TAB2-KDCATPUB-FROM(TAB-IX)              
382400               MOVE JA TO SW-OMRAKN-FROM                                  
382500               MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO KDCATPUB-NEW            
382600               MOVE TAB-IX TO UPPDAT-FROM-IX                              
382700            ELSE                                                          
382800               MOVE MFS-ALFA-FAELT-FEL                                    
382900                              TO MOD-KDCATPUB-R-TOM-ATTR                  
383000               MOVE JA TO INDATA-FEL                                      
383100            END-IF                                                        
383200          End-if                                                          
383300        END-IF                                                            
383400     END-IF                                                               
383500                                                                          
383600     IF SW-NYUPPLAGG-NYTT = JA                                            
383700        IF MID-KDCATPUB-R-TOM = ALL '+'                                   
383800           MOVE HIGH-VALUE TO TEST-KDCATPUB-TOM                           
383900        ELSE                                                              
384000           MOVE WS-KDCATPUB-TOM TO TEST-KDCATPUB-TOM                      
384100        END-IF                                                            
384200        MOVE WS-KDCATPUB TO test-KDCATPUB-from                            
384300        IF TEST-KDCATPUB-TOM < TEST-KDCATPUB-FROM                         
384400           MOVE JA TO INDATA-FEL                                          
384500           MOVE MED-8(SPRAAK-IX) TO MOD-TEMFSINF                          
384600        END-IF                                                            
384700     END-IF                                                               
384800                                                                          
384900*******   KOLL AKTUELL PUBKOD                                             
385000     IF SW-BORTTAG = JA                                                   
385100        CONTINUE                                                          
385200     ELSE                                                                 
385300        IF TEST-IX > ZERO                                                 
385400           IF TEST-KDCATPUB-TOM < TEST-KDCATPUB-FROM                      
385500              MOVE JA TO INDATA-FEL                                       
385600              MOVE MED-8(SPRAAK-IX) TO MOD-TEMFSINF                       
385700           END-IF                                                         
385800        END-IF                                                            
385900     END-IF                                                               
386000                                                                          
386100*******   KOLL OMRAKNAD PUBKOD-FROM                                       
386200     MOVE NEJ TO GENERAL-KOLL-FROM                                        
386300     IF SW-OMRAKN-FROM = JA                                               
386400        IF TAB2-KDCATPUB-FROM(UPPDAT-FROM-IX) = LOW-VALUE                 
386500           CONTINUE                                                       
386600        ELSE                                                              
386700           PERFORM IMS-GU-KAT                                             
386800           IF SEGMENT-FINNS                                               
386900              PERFORM IMS-GNP-KATM11                                      
387000              PERFORM UNTIL SEGMENT-SAKNAS                                
387100                 MOVE +1 TO PER-IX                                        
387200                 PERFORM UNTIL PER-IX > PER-IX-MAX                        
387300                    IF TAB-KDCATPUB-FOM(PER-IX) =                         
387400                       TAB2-KDCATPUB-FROM(UPPDAT-FROM-IX)                 
387500                       MOVE JA TO GENERAL-KOLL-FROM                       
387600                    END-IF                                                
387700                    ADD +1 TO PER-IX                                      
387800                 END-PERFORM                                              
387900                 PERFORM IMS-GNP-KATM11                                   
388000              END-PERFORM                                                 
388100              IF GENERAL-KOLL-FROM = NEJ                                  
388200                 MOVE JA TO INDATA-FEL                                    
388300                 MOVE MED-8(SPRAAK-IX) TO MOD-TEMFSINF                    
388400              END-IF                                                      
388500           END-IF                                                         
388600        END-IF                                                            
388700      END-IF                                                              
388800*******   KOLL OMRAKNAD PUBKOD-TOM                                        
388900     MOVE NEJ TO GENERAL-KOLL-TOM                                         
389000     IF SW-OMRAKN-TOM = JA                                                
389100        IF TAB2-KDCATPUB-TOM(UPPDAT-TOM-IX) = HIGH-VALUE                  
389200           CONTINUE                                                       
389300        ELSE                                                              
389400           PERFORM IMS-GU-KAT                                             
389500           IF SEGMENT-FINNS                                               
389600              PERFORM IMS-GNP-KATM11                                      
389700              PERFORM UNTIL SEGMENT-SAKNAS                                
389800                 MOVE +1 TO PER-IX                                        
389900                 PERFORM UNTIL PER-IX > PER-IX-MAX                        
390000                    IF TAB-KDCATPUB-TOM(PER-IX) =                         
390100                       TAB2-KDCATPUB-TOM(UPPDAT-TOM-IX)                   
390200                       MOVE JA TO GENERAL-KOLL-TOM                        
390300                    END-IF                                                
390400                    ADD +1 TO PER-IX                                      
390500                 END-PERFORM                                              
390600                 PERFORM IMS-GNP-KATM11                                   
390700              END-PERFORM                                                 
390800              IF GENERAL-KOLL-TOM = NEJ                                   
390900                 MOVE JA TO INDATA-FEL                                    
391000                 MOVE MED-8(SPRAAK-IX) TO MOD-TEMFSINF                    
391100              END-IF                                                      
391200           END-IF                                                         
391300        END-IF                                                            
391400     END-IF                                                               
391500                                                                          
391600     .                                                                    
391700     EJECT                                                                
391800 S04-KOLLA-GILTIG-KATPUB SECTION.                                         
391900                                                                          
392000     MOVE NEJ TO GODK-KATPUB-FINNS                                        
392100     IF TEST-KDCATPUB-TOM = HIGH-VALUE                                    
392200        MOVE JA TO GODK-KATPUB-FINNS                                      
392300     ELSE                                                                 
392400        MOVE W-IDCATNR TO W-IDCATNR-WDN1                                  
392500        PERFORM IMS-GU-KAT                                                
392600        IF SEGMENT-FINNS                                                  
392700           PERFORM IMS-GNP-KATM11                                         
392800           PERFORM UNTIL SEGMENT-SAKNAS                                   
392900              MOVE +1 TO PER-IX                                           
393000              PERFORM UNTIL PER-IX > PER-IX-MAX                           
393100                 IF TAB-KDCATPUB-TOM(PER-IX) =                            
393200                     WS-KDCATPUB-TOM                                      
393300                          MOVE JA TO GODK-KATPUB-FINNS                    
393400                 END-IF                                                   
393500                 ADD +1 TO PER-IX                                         
393600              END-PERFORM                                                 
393700              PERFORM IMS-GNP-KATM11                                      
393800           END-PERFORM                                                    
393900        END-IF                                                            
394000     END-IF                                                               
394100                                                                          
394200     IF GODK-KATPUB-FINNS = NEJ                                           
394300        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-TOM-ATTR                
394400        MOVE JA TO INDATA-FEL                                             
394500     END-IF                                                               
394600     .                                                                    
394700     EJECT                                                                
394800 S10-OMNUM-KDCATPUB-TOM SECTION.                                          
394900*    WDN512                                                               
395000     IF SW-OMRAKN-TOM = JA                                                
395100        MOVE UPPDAT-TOM-IX TO TAB-IX                                      
395200        PERFORM IMS-GHU-AVS                                               
395300        MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO W-KDCATPUB                     
395400                                                                          
395500        MOVE +1 TO W-IDCATRAD                                             
395600        PERFORM IMS-GHNP-AVS-RAD                                          
395700        PERFORM UNTIL W-IDCATRAD > 14                                     
395800           IF SEGMENT-FINNS                                               
395900              MOVE TAB2-KDCATPUB-TOM(TAB-IX) TO RAD-KDCATPUB-TOM          
396000              PERFORM IMS-REPL-AVS                                        
396100           END-IF                                                         
396200           ADD +1 TO W-IDCATRAD                                           
396300           PERFORM IMS-GHNP-AVS-RAD                                       
396400        END-PERFORM                                                       
396500                                                                          
396600*    WDN513 VADIS-VARIANTNYCKLAR LIKADANT                                 
397000        MOVE TAB2-KDCATPUB-FROM(TAB-IX) TO W-KDCATPUB-F13                 
397100                                                                          
397300        PERFORM IMS-GHNP-AVS-VADIS                                        
397400        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
397600           MOVE TAB2-KDCATPUB-TOM(TAB-IX) TO VADIS-KDCATPUB-TOM           
397700           PERFORM IMS-REPL-AVS                                           
398000           PERFORM IMS-GHNP-AVS-VADIS                                     
398100        END-PERFORM                                                       
398200     END-IF                                                               
398300     .                                                                    
398400     EJECT                                                                
398500 S11-OMNUM-KDCATPUB-FROM SECTION.                                         
398600                                                                          
398700     IF KDCATPUB-NEW = KDCATPUB-OLD                                       
398800        CONTINUE                                                          
398900     ELSE                                                                 
399000        MOVE NEJ TO GALLANDE-FINNS                                        
399100        IF SW-OMRAKN-FROM = JA                                            
399200           PERFORM IMS-GU-AVS                                             
399300           PERFORM IMS-GHNP-AVS-ILLU                                      
399400           PERFORM UNTIL SEGMENT-SAKNAS OR GALLANDE-FINNS = JA            
399500             IF ILLU-KDCATPUB-FOM = KDCATPUB-OLD                          
399600                PERFORM IMS-DLET-AVS                                      
399700                MOVE JA TO GALLANDE-FINNS                                 
399800                MOVE KDCATPUB-NEW TO ILLU-KDCATPUB-FOM                    
399900                PERFORM IMS-ISRT-AVS-ILLU-PCB2                            
400000             END-IF                                                       
400100             PERFORM IMS-GHNP-AVS-ILLU                                    
400200           END-PERFORM                                                    
400300                                                                          
400400           MOVE +1 TO W-IDCATRAD                                          
400500           MOVE KDCATPUB-OLD TO W-KDCATPUB                                
400600           PERFORM IMS-GHNP-AVS-RAD                                       
400700           IF SEGMENT-FINNS                                               
400800              MOVE KDCATPUB-NEW TO RAD-KDCATPUB-FOM                       
400810                                   W-KDCATPUB                             
400900              MOVE DAGENS-DATUM TO RAD-TIUPPDAT                           
401000              MOVE 'FRÅN S11-OMNUM- SECTION.'                             
401010                                 TO ABENDINFO                             
401100              PERFORM IMS-ISRT-AVS-RAD-PCB2                               
401200                                                                          
401300              MOVE KDCATPUB-OLD TO W-KDCATPUB                             
401400              PERFORM IMS-GHNP-AVS-RUB                                    
401500              PERFORM UNTIL SEGMENT-SAKNAS                                
401600                 MOVE KDCATPUB-NEW TO W-KDCATPUB                          
401700                 PERFORM IMS-ISRT-AVS-RUB-PCB2                            
401800                 PERFORM IMS-DLET-AVS                                     
401900                 MOVE KDCATPUB-OLD TO W-KDCATPUB                          
402000                 PERFORM IMS-GHNP-AVS-RUB                                 
402100              END-PERFORM                                                 
402200                                                                          
402300              MOVE +1 TO W-IDCATRAD                                       
402400              MOVE KDCATPUB-OLD TO W-KDCATPUB                             
402500              PERFORM IMS-GHNP-AVS-RAD-FIRST                              
402600              PERFORM IMS-DLET-AVS                                        
402700           END-IF                                                         
402800                                                                          
402900           MOVE +4 TO W-IDCATRAD                                          
403000           MOVE KDCATPUB-OLD TO W-KDCATPUB                                
403100           PERFORM IMS-GHNP-AVS-RAD                                       
403200           IF SEGMENT-FINNS                                               
403300              MOVE KDCATPUB-NEW TO RAD-KDCATPUB-FOM                       
403400              MOVE DAGENS-DATUM TO RAD-TIUPPDAT                           
403500              MOVE KDCATPUB-OLD TO W-KDCATPUB                             
403510              MOVE 'FRÅN S11-OMNUM- SECTION.'                             
403520                                 TO ABENDINFO                             
403600              PERFORM IMS-ISRT-AVS-RAD-PCB2                               
403700                                                                          
403800              MOVE KDCATPUB-OLD TO W-KDCATPUB                             
403900              PERFORM IMS-GHNP-AVS-TEXT                                   
404000              IF SEGMENT-FINNS                                            
404100                 MOVE KDCATPUB-NEW TO W-KDCATPUB                          
404200                 PERFORM IMS-ISRT-AVS-TEXT-PCB2                           
404300              END-IF                                                      
404400                                                                          
404500              MOVE KDCATPUB-OLD TO W-KDCATPUB                             
404600              PERFORM IMS-GHNP-AVS-FOT                                    
404700              PERFORM UNTIL SEGMENT-SAKNAS                                
404800                 MOVE KDCATPUB-NEW TO W-KDCATPUB                          
404900                 PERFORM IMS-ISRT-AVS-FOT-PCB2                            
405000                 PERFORM IMS-DLET-AVS                                     
405100                 MOVE KDCATPUB-OLD TO W-KDCATPUB                          
405200                 PERFORM IMS-GHNP-AVS-FOT                                 
405300              END-PERFORM                                                 
405400                                                                          
405500              MOVE +4 TO W-IDCATRAD                                       
405600              MOVE KDCATPUB-OLD TO W-KDCATPUB                             
405700              PERFORM IMS-GHNP-AVS-RAD-FIRST                              
405800              PERFORM IMS-DLET-AVS                                        
405900           END-IF                                                         
406000                                                                          
406100           MOVE +5 TO W-IDCATRAD                                          
406200           MOVE KDCATPUB-OLD TO W-KDCATPUB                                
406300           PERFORM IMS-GHNP-AVS-RAD                                       
406400           IF SEGMENT-FINNS                                               
406500              MOVE KDCATPUB-NEW TO RAD-KDCATPUB-FOM                       
406600              MOVE DAGENS-DATUM TO RAD-TIUPPDAT                           
406700              MOVE KDCATPUB-NEW TO W-KDCATPUB                             
406710              MOVE 'FRÅN S11-OMNUM- SECTION.'                             
406720                                 TO ABENDINFO                             
406800              PERFORM IMS-ISRT-AVS-RAD-PCB2                               
406900                                                                          
407000              MOVE KDCATPUB-OLD TO W-KDCATPUB                             
407100              PERFORM IMS-GHNP-AVS-TEXT                                   
407200              IF SEGMENT-FINNS                                            
407300                 MOVE KDCATPUB-NEW TO W-KDCATPUB                          
407400                 PERFORM IMS-ISRT-AVS-TEXT-PCB2                           
407500                 MOVE KDCATPUB-OLD TO W-KDCATPUB                          
407600                 PERFORM IMS-DLET-AVS                                     
407700              END-IF                                                      
407800                                                                          
407900              MOVE +5 TO W-IDCATRAD                                       
408000              MOVE KDCATPUB-OLD TO W-KDCATPUB                             
408100              PERFORM IMS-GHNP-AVS-RAD-FIRST                              
408200              PERFORM IMS-DLET-AVS                                        
408300           END-IF                                                         
408400                                                                          
408500           MOVE +10 TO W-IDCATRAD                                         
408600           MOVE KDCATPUB-OLD TO W-KDCATPUB                                
408700           PERFORM IMS-GHNP-AVS-RAD                                       
408800           PERFORM UNTIL W-IDCATRAD = 15                                  
408900             IF SEGMENT-FINNS                                             
409000                MOVE KDCATPUB-NEW TO RAD-KDCATPUB-FOM                     
409100                MOVE DAGENS-DATUM TO RAD-TIUPPDAT                         
409200                MOVE KDCATPUB-NEW TO W-KDCATPUB                           
409210                MOVE 'FRÅN S11-OMNUM- SECTION.'                           
409220                                 TO ABENDINFO                             
409300                PERFORM IMS-ISRT-AVS-RAD-PCB2                             
409400                                                                          
409500                MOVE KDCATPUB-OLD TO W-KDCATPUB                           
409600                PERFORM IMS-GHNP-AVS-TEXT                                 
409700                IF SEGMENT-FINNS                                          
409800                   MOVE KDCATPUB-NEW TO W-KDCATPUB                        
409900                   PERFORM IMS-ISRT-AVS-TEXT-PCB2                         
410000                   MOVE KDCATPUB-OLD TO W-KDCATPUB                        
410100                   PERFORM IMS-DLET-AVS                                   
410200                END-IF                                                    
410300                                                                          
410400                MOVE KDCATPUB-OLD TO W-KDCATPUB                           
410500                PERFORM IMS-GHNP-AVS-FOT                                  
410600                PERFORM UNTIL SEGMENT-SAKNAS                              
410700                   MOVE KDCATPUB-NEW TO W-KDCATPUB                        
410800                   PERFORM IMS-ISRT-AVS-FOT-PCB2                          
410900                   PERFORM IMS-DLET-AVS                                   
411000                   MOVE KDCATPUB-OLD TO W-KDCATPUB                        
411100                   PERFORM IMS-GHNP-AVS-FOT                               
411200                END-PERFORM                                               
411300                                                                          
411320                MOVE W-WDN512KY-X  TO W-WDN5GSEQ-12-X                     
411400                PERFORM IMS-GU-AVSG-HAEN                                  
411500                PERFORM UNTIL SEGMENT-SAKNAS                              
411600                   PERFORM S99-FIXA-HANVISNING                            
411610*              --- Tag bort hänvisnings-segm i hänvisnande avsnitt        
411620*              --- RAD-AVS3 läst med GU i S99-FIXA..                      
411700                   PERFORM IMS-GHNP-AVS-HAEN-AVS3                         
411710                   PERFORM IMS-DLET-AVS3                                  
411800                   PERFORM IMS-GN-AVSG-HAEN                               
411900                END-PERFORM                                               
412000                                                                          
412100                MOVE KDCATPUB-OLD TO W-KDCATPUB                           
412200                PERFORM IMS-GHNP-AVS-RAD-FIRST                            
412300                PERFORM IMS-DLET-AVS                                      
412400             END-IF                                                       
412500                                                                          
412600             ADD +1 TO W-IDCATRAD                                         
412700             MOVE KDCATPUB-OLD TO W-KDCATPUB                              
412800             PERFORM IMS-GHNP-AVS-RAD                                     
412900           END-PERFORM                                                    
412910           PERFORM S11A-OMNUM-VADIS-FROM                                  
413000        END-IF                                                            
413100     END-IF                                                               
413200     .                                                                    
413300     EJECT                                                                
413310 S11A-OMNUM-VADIS-FROM SECTION.                                           
413311     SKIP2                                                                
413312*    --- Byt även KDCATPUB-FOM på VADIS-segmenten WDN513                  
413313     PERFORM IMS-GU-AVS                                                   
413314     MOVE KDCATPUB-OLD TO W-KDCATPUB-F13                                  
413315     PERFORM IMS-GHNP-AVS-VADIS                                           
413316     PERFORM UNTIL SEGMENT-SAKNAS                                         
413318       MOVE KDCATPUB-NEW TO VADIS-KDCATPUB-FOM                            
413319                                                                          
413321       PERFORM IMS-ISRT-AVS-VADIS-PCB2                                    
413322                                                                          
413323       MOVE KDCATPUB-OLD TO W-KDCATPUB-F13                                
413324       PERFORM IMS-GHNP-AVS-VADIS-FIRST                                   
413336       PERFORM IMS-DLET-AVS                                               
413338       PERFORM IMS-GHNP-AVS-VADIS                                         
413339     END-PERFORM                                                          
413340     .                                                                    
413350     EJECT                                                                
413400 S99-FIXA-HANVISNING SECTION.                                             
413500     SKIP2                                                                
413510*    --- Skriv radadressen för hänvisningen som skall tas bort            
413520*    --- på notsegmentet i hänvisande avsnitt.                            
413600     MOVE AVSG-IDWDN512         TO AVSG-REF-IDWDN512                      
413610     MOVE AVSG-REF-IDCATNR      TO W-IDCATNR-H                            
413700     MOVE AVSG-REF-IDCATGRP     TO W-IDCATGRP-H                           
413800     MOVE AVSG-REF-IDCATAVS     TO W-IDCATAVS-H                           
413900     MOVE AVSG-REF-IDCATRAD     TO W-IDCATRAD-H                           
414000     MOVE AVSG-REF-KDCATPUB-FOM TO W-KDCATPUB-H                           
414100     PERFORM IMS-GU-AVS-RAD-AVS3                                          
414200     IF SEGMENT-FINNS                                                     
414300        MOVE +2 TO W-IDSEGMNR                                             
414400        PERFORM IMS-GHNP-AVS-NOT-AVS3                                     
414500        IF SEGMENT-SAKNAS                                                 
414600           MOVE +2                      TO NOT-IDSEGMNR                   
414700           MOVE W-IDCATGRP              TO NOT-IDCATGRP                   
414800           MOVE W-IDCATAVS              TO NOT-IDCATAVS                   
414900           MOVE W-IDCATRAD              TO NOT-IDCATRAD                   
415000           MOVE W-KDCATPUB              TO NOT-KDCATPUB-FOM               
415100           MOVE HANVISN-KDCATPUB-TOM TO NOT-KDCATPUB-TOM                  
415200           PERFORM IMS-ISRT-AVS-NOT-AVS3                                  
415300        END-IF                                                            
415400     END-IF                                                               
415500     .                                                                    
415510     EJECT                                                                
415520*                                                                         
415530* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
415540* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
415550*                                                                         
415560*    -COPY W150Y2K1                                                       
415600     EJECT                                                                
415700* IMS SEKTIONER                                                           
415800     SKIP1                                                                
415900 IMS-GET-MSG SECTION.                                                     
416000     MOVE '  QC' TO GODK-STATUSKODER                                      
416100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
416200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
416300     PERFORM IMS-STATUSKONTROLL                                           
416400     .                                                                    
416500     SKIP3                                                                
416600 IMS-INSERT-MSG SECTION.                                                  
416700     IF ENGLISH-TEXT                                                      
416800       MOVE 'N' TO MFS-KDHUVOMR                                           
416900     END-IF                                                               
417000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
417100     MOVE SPACE TO GODK-STATUSKODER                                       
417200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
417300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
417400     PERFORM IMS-STATUSKONTROLL                                           
417500     .                                                                    
417600     EJECT                                                                
417700 IMS-GU-AVS SECTION.                                                      
417800     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
417900            DELIMITED BY SIZE INTO SSA1                                   
418000     MOVE '  GE' TO GODK-STATUSKODER                                      
418100     CALL CBLTDLI USING GU AVS-PCB IO-AREA-1 SSA1                         
418200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
418300     PERFORM IMS-STATUSKONTROLL                                           
418400     .                                                                    
418500     SKIP3                                                                
418600 IMS-GHU-AVS SECTION.                                                     
418700     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
418800            DELIMITED BY SIZE INTO SSA1                                   
418900     MOVE '  GE' TO GODK-STATUSKODER                                      
419000     CALL CBLTDLI USING GHU AVS-PCB IO-AREA-1 SSA1                        
419100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
419200     PERFORM IMS-STATUSKONTROLL                                           
419300     .                                                                    
419400     SKIP3                                                                
419500 IMS-ISRT-AVS SECTION.                                                    
419600     MOVE 'WLKATH01 ' TO SSA1                                             
419700     MOVE '  II' TO GODK-STATUSKODER                                      
419800     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1                       
419900     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
420000     PERFORM IMS-STATUSKONTROLL                                           
420100     .                                                                    
420200     EJECT                                                                
420300 IMS-GNP-AVS-ILLU SECTION.                                                
420400     MOVE 'WLKATH11 ' TO SSA1                                             
420500     MOVE '  GE' TO GODK-STATUSKODER                                      
420600     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
420700     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
420800     PERFORM IMS-STATUSKONTROLL                                           
420900     .                                                                    
421000     SKIP2                                                                
421100 IMS-GET-AVS-RAD SECTION.                                                 
421200     MOVE 'WLKATH12 ' TO SSA1                                             
421300     MOVE '  GE' TO GODK-STATUSKODER                                      
421400     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
421500     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
421600     PERFORM IMS-STATUSKONTROLL                                           
421700     .                                                                    
421800     SKIP2                                                                
421900 IMS-GHNP-AVS-ILLU SECTION.                                               
422000     MOVE 'WLKATH11 ' TO SSA1                                             
422100     MOVE '  GE' TO GODK-STATUSKODER                                      
422200     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1                       
422300     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
422400     PERFORM IMS-STATUSKONTROLL                                           
422500     .                                                                    
422600     SKIP2                                                                
422700 IMS-ISRT-AVS-ILLU SECTION.                                               
422800     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
422900            DELIMITED BY SIZE INTO SSA1                                   
423000     MOVE 'WLKATH11 ' TO SSA2                                             
423100     MOVE '  ' TO GODK-STATUSKODER                                        
423200     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
423300     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
423400     PERFORM IMS-STATUSKONTROLL                                           
423500     .                                                                    
423600     EJECT                                                                
423700 IMS-ISRT-AVS-ILLU-PCB2 SECTION.                                          
423800     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
423900            DELIMITED BY SIZE INTO SSA1                                   
424000     MOVE 'WLKATH11 ' TO SSA2                                             
424100     MOVE '  ' TO GODK-STATUSKODER                                        
424200     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
424300     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
424400     PERFORM IMS-STATUSKONTROLL                                           
424500     .                                                                    
424600     SKIP2                                                                
424700 IMS-GNP-AVS-RAD SECTION.                                                 
424800     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
424900            DELIMITED BY SIZE INTO SSA1                                   
425000     MOVE '  GE' TO GODK-STATUSKODER                                      
425100     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
425200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
425300     PERFORM IMS-STATUSKONTROLL                                           
425400     .                                                                    
425500     SKIP2                                                                
425600 IMS-GHNP-AVS-RAD SECTION.                                                
425700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
425800            DELIMITED BY SIZE INTO SSA1                                   
425900     MOVE '  GE' TO GODK-STATUSKODER                                      
426000     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1                       
426100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
426200     PERFORM IMS-STATUSKONTROLL                                           
426300     .                                                                    
426400     EJECT                                                                
426500 IMS-GNP-AVS-RAD-SOEK-FIRST SECTION.                                      
426600     STRING 'WLKATH12*F(WDN512KY=>' W-WDN512KY-MIN                        
426700                    '&WDN512KY<=' W-WDN512KY-MAX ')'                      
426800            DELIMITED BY SIZE INTO SSA1                                   
426900     MOVE '  GE' TO GODK-STATUSKODER                                      
427000     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
427100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
427200     PERFORM IMS-STATUSKONTROLL                                           
427300     .                                                                    
427400     SKIP2                                                                
427500 IMS-GNP-AVS-RAD-NASTA-FIRST SECTION.                                     
427600     STRING 'WLKATH12*F(WDN512KY >' W-WDN512KY-MIN                        
427700                    '&WDN512KY< ' W-WDN512KY-MAX ')'                      
427800            DELIMITED BY SIZE INTO SSA1                                   
427900     MOVE '  GE' TO GODK-STATUSKODER                                      
428000     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
428100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
428200     PERFORM IMS-STATUSKONTROLL                                           
428300     .                                                                    
428400     SKIP2                                                                
428500 IMS-GNP-AVS-RAD-SOEK SECTION.                                            
428600     STRING 'WLKATH12(WDN512KY=>' W-WDN512KY-MIN                          
428700                    '&WDN512KY<=' W-WDN512KY-MAX ')'                      
428800            DELIMITED BY SIZE INTO SSA1                                   
428900     MOVE '  GE' TO GODK-STATUSKODER                                      
429000     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
429100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
429200     PERFORM IMS-STATUSKONTROLL                                           
429300     .                                                                    
429400     EJECT                                                                
429500 IMS-GNP-AVS-RAD-NASTA SECTION.                                           
429600     STRING 'WLKATH12(WDN512KY >' W-WDN512KY-MIN                          
429700                    '&WDN512KY< ' W-WDN512KY-MAX ')'                      
429800            DELIMITED BY SIZE INTO SSA1                                   
429900     MOVE '  GE' TO GODK-STATUSKODER                                      
430000     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
430100     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
430200     PERFORM IMS-STATUSKONTROLL                                           
430300     .                                                                    
430400     SKIP2                                                                
430500 IMS-GHNP-AVS-RAD-FIRST SECTION.                                          
430600     STRING 'WLKATH12*F(WDN512KY =' W-WDN512KY-X ')'                      
430700            DELIMITED BY SIZE INTO SSA1                                   
430800     MOVE '  GE' TO GODK-STATUSKODER                                      
430900     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1                       
431000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
431100     PERFORM IMS-STATUSKONTROLL                                           
431200     .                                                                    
431300     EJECT                                                                
431400 IMS-ISRT-AVS-RAD SECTION.                                                
431410     MOVE 'IMS-ISRT-AVS-RAD ' TO FELTEXT                                  
431500     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
431600            DELIMITED BY SIZE INTO SSA1                                   
431700     MOVE 'WLKATH12 ' TO SSA2                                             
431800     MOVE '  ' TO GODK-STATUSKODER                                        
431900     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
432000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
432100     PERFORM IMS-STATUSKONTROLL                                           
432200     .                                                                    
432300     SKIP2                                                                
432310 IMS-GHNP-AVS-VADIS SECTION.                                              
432320     STRING 'WLKATH13(KDCATPUF =' W-KDCATPUB-F13 ')'                      
432330            DELIMITED BY SIZE INTO SSA1                                   
432340     MOVE '  GE' TO GODK-STATUSKODER                                      
432350     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1                       
432360     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
432370     PERFORM IMS-STATUSKONTROLL                                           
432380     .                                                                    
432390     EJECT                                                                
432391 IMS-GHNP-AVS-VADIS-FIRST SECTION.                                        
432392     STRING 'WLKATH13*F(KDCATPUF =' W-KDCATPUB-F13 ')'                    
432393            DELIMITED BY SIZE INTO SSA1                                   
432394     MOVE '  GE' TO GODK-STATUSKODER                                      
432395     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1                       
432396     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
432397     PERFORM IMS-STATUSKONTROLL                                           
432398     .                                                                    
432399     EJECT                                                                
432400 IMS-ISRT-AVS-RAD-PCB2  SECTION.                                          
432410     MOVE 'IMS-ISRT-AVS-RAD-PCB2 ' TO FELTEXT                             
432500     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
432600            DELIMITED BY SIZE INTO SSA1                                   
432700     MOVE 'WLKATH12 ' TO SSA2                                             
432800     MOVE '  ' TO GODK-STATUSKODER                                        
432900     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
433000     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
433100     PERFORM IMS-STATUSKONTROLL                                           
433200     .                                                                    
433300     EJECT                                                                
433310 IMS-ISRT-AVS-VADIS-PCB2     SECTION.                                     
433320     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
433330            DELIMITED BY SIZE INTO SSA1                                   
433340     MOVE 'WLKATH13 ' TO SSA2                                             
433350     MOVE '  ' TO GODK-STATUSKODER                                        
433360     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
433370     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
433380     PERFORM IMS-STATUSKONTROLL                                           
433390     .                                                                    
433391     EJECT                                                                
433400 IMS-GNP-AVS-TEXT SECTION.                                                
433500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
433600            DELIMITED BY SIZE INTO SSA1                                   
433700     MOVE 'WLKATH22 ' TO SSA2                                             
433800     MOVE '  GE' TO GODK-STATUSKODER                                      
433900     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
434000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
434100     PERFORM IMS-STATUSKONTROLL                                           
434200     .                                                                    
434300     SKIP2                                                                
434400 IMS-GHNP-AVS-TEXT SECTION.                                               
434500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
434600            DELIMITED BY SIZE INTO SSA1                                   
434700     MOVE 'WLKATH22 ' TO SSA2                                             
434800     MOVE '  GE' TO GODK-STATUSKODER                                      
434900     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
435000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
435100     PERFORM IMS-STATUSKONTROLL                                           
435200     .                                                                    
435300     SKIP2                                                                
435400 IMS-ISRT-AVS-TEXT-PCB2 SECTION.                                          
435500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
435600            DELIMITED BY SIZE INTO SSA1                                   
435700     MOVE 'WLKATH22 ' TO SSA2                                             
435800     MOVE '  ' TO GODK-STATUSKODER                                        
435900     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
436000     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
436100     PERFORM IMS-STATUSKONTROLL                                           
436200     .                                                                    
436300     EJECT                                                                
436400 IMS-ISRT-AVS-TEXT SECTION.                                               
436500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
436600            DELIMITED BY SIZE INTO SSA1                                   
436700     MOVE 'WLKATH22 ' TO SSA2                                             
436800     MOVE '  ' TO GODK-STATUSKODER                                        
436900     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
437000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
437100     PERFORM IMS-STATUSKONTROLL                                           
437200     .                                                                    
437300     SKIP2                                                                
437400 IMS-GNP-AVS-RUB SECTION.                                                 
437500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
437600            DELIMITED BY SIZE INTO SSA1                                   
437700     MOVE 'WLKATH25 ' TO SSA2                                             
437800     MOVE '  GE' TO GODK-STATUSKODER                                      
437900     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
438000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
438100     PERFORM IMS-STATUSKONTROLL                                           
438200     .                                                                    
438300     SKIP2                                                                
438400 IMS-GHNP-AVS-RUB SECTION.                                                
438500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
438600            DELIMITED BY SIZE INTO SSA1                                   
438700     MOVE 'WLKATH25 ' TO SSA2                                             
438800     MOVE '  GE' TO GODK-STATUSKODER                                      
438900     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
439000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
439100     PERFORM IMS-STATUSKONTROLL                                           
439200     .                                                                    
439300     EJECT                                                                
439400 IMS-ISRT-AVS-RUB-PCB2 SECTION.                                           
439500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
439600            DELIMITED BY SIZE INTO SSA1                                   
439700     MOVE 'WLKATH25 ' TO SSA2                                             
439800     MOVE '  ' TO GODK-STATUSKODER                                        
439900     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
440000     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
440100     PERFORM IMS-STATUSKONTROLL                                           
440200     .                                                                    
440300     SKIP2                                                                
440400 IMS-ISRT-AVS-RUB SECTION.                                                
440500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
440600            DELIMITED BY SIZE INTO SSA1                                   
440700     MOVE 'WLKATH25 ' TO SSA2                                             
440800     MOVE '  ' TO GODK-STATUSKODER                                        
440900     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
441000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
441100     PERFORM IMS-STATUSKONTROLL                                           
441200     .                                                                    
441300     SKIP2                                                                
441400 IMS-GNP-AVS-FOT SECTION.                                                 
441500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
441600            DELIMITED BY SIZE INTO SSA1                                   
441700     MOVE 'WLKATH26 ' TO SSA2                                             
441800     MOVE '  GE' TO GODK-STATUSKODER                                      
441900     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
442000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
442100     PERFORM IMS-STATUSKONTROLL                                           
442200     .                                                                    
442300     EJECT                                                                
442310 IMS-GU-AVSG-HAEN SECTION.                                                
442311     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5G1KY-HAEN                         
442312                                  W-IDCATRKY-LO                           
442313                 OCH 'WDN5G1KY<=' W-WDN5G1KY-HAEN                         
442314                                  W-IDCATRKY-HI ')'                       
442315            DELIMITED BY SIZE INTO SSA1                                   
442350     MOVE '  GE' TO GODK-STATUSKODER                                      
442360     CALL CBLTDLI USING GU KATS-PCB AVSG-IO-AREA SSA1                     
442370     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
442380     PERFORM IMS-STATUSKONTROLL                                           
442390     .                                                                    
442391     SKIP2                                                                
442392 IMS-GN-AVSG-HAEN SECTION.                                                
442393     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5G1KY-HAEN                         
442394                                  W-IDCATRKY-LO                           
442395                 OCH 'WDN5G1KY<=' W-WDN5G1KY-HAEN                         
442396                                  W-IDCATRKY-HI ')'                       
442397            DELIMITED BY SIZE INTO SSA1                                   
442401     MOVE '  GE' TO GODK-STATUSKODER                                      
442402     CALL CBLTDLI USING GN KATS-PCB AVSG-IO-AREA SSA1                     
442403     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
442404     PERFORM IMS-STATUSKONTROLL                                           
442405     .                                                                    
442406*    EJECT                                                                
442410*IMS-GNP-AVS-REF SECTION.                                                 
442500*    STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
442600*           DELIMITED BY SIZE INTO SSA1                                   
442700*    MOVE 'WLKATH28 ' TO SSA2                                             
442800*    MOVE '  GE' TO GODK-STATUSKODER                                      
442900*    CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
443000*    MOVE AVS-STATUS-CODE TO STATUS-WS                                    
443100*    PERFORM IMS-STATUSKONTROLL                                           
443200*    .                                                                    
443300     EJECT                                                                
443400 IMS-GHNP-AVS-FOT SECTION.                                                
443500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
443600            DELIMITED BY SIZE INTO SSA1                                   
443700     MOVE 'WLKATH26 ' TO SSA2                                             
443800     MOVE '  GE' TO GODK-STATUSKODER                                      
443900     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA-1 SSA1 SSA2                  
444000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
444100     PERFORM IMS-STATUSKONTROLL                                           
444200     .                                                                    
444300     SKIP2                                                                
444400 IMS-ISRT-AVS-FOT SECTION.                                                
444500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
444600            DELIMITED BY SIZE INTO SSA1                                   
444700     MOVE 'WLKATH26 ' TO SSA2                                             
444800     MOVE '  ' TO GODK-STATUSKODER                                        
444900     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA-1 SSA1 SSA2                  
445000     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
445100     PERFORM IMS-STATUSKONTROLL                                           
445200     .                                                                    
445300     EJECT                                                                
445400 IMS-ISRT-AVS-FOT-PCB2 SECTION.                                           
445500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
445600            DELIMITED BY SIZE INTO SSA1                                   
445700     MOVE 'WLKATH26 ' TO SSA2                                             
445800     MOVE '  ' TO GODK-STATUSKODER                                        
445900     CALL CBLTDLI USING ISRT AVS2-PCB IO-AREA-1 SSA1 SSA2                 
446000     MOVE AVS2-STATUS-CODE TO STATUS-WS                                   
446100     PERFORM IMS-STATUSKONTROLL                                           
446200     .                                                                    
446300     SKIP2                                                                
446400 IMS-REPL-AVS SECTION.                                                    
446500     MOVE '  ' TO GODK-STATUSKODER                                        
446600     CALL CBLTDLI USING REPL AVS-PCB IO-AREA-1                            
446700     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
446800     PERFORM IMS-STATUSKONTROLL                                           
446900     .                                                                    
447000     SKIP2                                                                
447100 IMS-DLET-AVS SECTION.                                                    
447200     MOVE '  ' TO GODK-STATUSKODER                                        
447300     CALL CBLTDLI USING DLET AVS-PCB IO-AREA-1                            
447400     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
447500     PERFORM IMS-STATUSKONTROLL                                           
447600     .                                                                    
447700     EJECT                                                                
447800 IMS-GU-RUB SECTION.                                                      
447900     STRING 'WLKATB01(IDRUBNR  =' W-IDRUBNR-X ')'                         
448000            DELIMITED BY SIZE INTO SSA1                                   
448100     MOVE '  GE' TO GODK-STATUSKODER                                      
448200     CALL CBLTDLI USING GU RUB-PCB IO-AREA-2 SSA1                         
448300     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
448400     PERFORM IMS-STATUSKONTROLL                                           
448500     .                                                                    
448600     SKIP2                                                                
448700 IMS-GU-FOT SECTION.                                                      
448800     STRING 'WLKATF01(IDFOTNR  =' W-IDFOTNR-X ')'                         
448900            DELIMITED BY SIZE INTO SSA1                                   
449000     MOVE '  GE' TO GODK-STATUSKODER                                      
449100     CALL CBLTDLI USING GU FOT-PCB IO-AREA-2 SSA1                         
449200     MOVE FOT-STATUS-CODE TO STATUS-WS                                    
449300     PERFORM IMS-STATUSKONTROLL                                           
449400     .                                                                    
449500     SKIP2                                                                
449600 IMS-GU-ILLU SECTION.                                                     
449700     STRING 'WLKATL01(IDILLU   =' W-IDILLU-X ')'                          
449800            DELIMITED BY SIZE INTO SSA1                                   
449900     MOVE '  GE' TO GODK-STATUSKODER                                      
450000     CALL CBLTDLI USING GU ILLU-PCB IO-AREA-2 SSA1                        
450100     MOVE ILLU-STATUS-CODE TO STATUS-WS                                   
450200     PERFORM IMS-STATUSKONTROLL                                           
450300     .                                                                    
450400     EJECT                                                                
450500 IMS-GU-KAT SECTION.                                                      
450600     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
450700            DELIMITED BY SIZE INTO SSA1                                   
450800     MOVE '  GE' TO GODK-STATUSKODER                                      
450900     CALL CBLTDLI USING GU KAT-PCB IO-AREA-2 SSA1                         
451000     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
451100     PERFORM IMS-STATUSKONTROLL                                           
451200     .                                                                    
451300     SKIP2                                                                
452300 IMS-GNP-KATM11 SECTION.                                                  
452400     MOVE 'WLKATM11 ' TO SSA1                                             
452500     MOVE '  GE' TO GODK-STATUSKODER                                      
452600     CALL CBLTDLI USING GNP KAT-PCB IO-AREA-2 SSA1                        
452700     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
452800     PERFORM IMS-STATUSKONTROLL                                           
452900     .                                                                    
453000     EJECT                                                                
453100 IMS-GU-AVS-RAD-AVS3 SECTION.                                             
453200     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-H-X ')'                      
453300            DELIMITED BY SIZE INTO SSA1                                   
453400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-H-X ')'                      
453500            DELIMITED BY SIZE INTO SSA2                                   
453600     MOVE '  GE' TO GODK-STATUSKODER                                      
453700     CALL CBLTDLI USING GU AVS3-PCB IO-AREA-1 SSA1 SSA2                   
453800     MOVE AVS3-STATUS-CODE TO STATUS-WS                                   
453900     PERFORM IMS-STATUSKONTROLL                                           
454000     .                                                                    
454100     SKIP2                                                                
454200 IMS-GHNP-AVS-NOT-AVS3 SECTION.                                           
454300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-H-X ')'                      
454400            DELIMITED BY SIZE INTO SSA1                                   
454500     STRING 'WLKATH24(IDSEGMNR =' W-IDSEGMNR-X ')'                        
454600            DELIMITED BY SIZE INTO SSA2                                   
454700     MOVE '  GE' TO GODK-STATUSKODER                                      
454800     CALL CBLTDLI USING GHNP AVS3-PCB IO-AREA-1 SSA1 SSA2                 
454900     MOVE AVS3-STATUS-CODE TO STATUS-WS                                   
455000     PERFORM IMS-STATUSKONTROLL                                           
455100     .                                                                    
455200     SKIP2                                                                
455210 IMS-GHNP-AVS-HAEN-AVS3 SECTION.                                          
455220     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-H-X ')'                      
455230            DELIMITED BY SIZE INTO SSA1                                   
455240     STRING 'WLKATH27(IDKATRKY =' W-WDN512KY-X ')'                        
455250            DELIMITED BY SIZE INTO SSA2                                   
455260     MOVE '  GE' TO GODK-STATUSKODER                                      
455270     CALL CBLTDLI USING GHNP AVS3-PCB IO-AREA-1 SSA1 SSA2                 
455280     MOVE AVS3-STATUS-CODE TO STATUS-WS                                   
455290     PERFORM IMS-STATUSKONTROLL                                           
455291     .                                                                    
455292     SKIP2                                                                
455293 IMS-DLET-AVS3 SECTION.                                                   
455294     MOVE '  ' TO GODK-STATUSKODER                                        
455295     CALL CBLTDLI USING DLET AVS3-PCB IO-AREA-1                           
455296     MOVE AVS3-STATUS-CODE TO STATUS-WS                                   
455297     PERFORM IMS-STATUSKONTROLL                                           
455298     .                                                                    
455299     EJECT                                                                
455300 IMS-ISRT-AVS-NOT-AVS3 SECTION.                                           
455400     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-H-X ')'                      
455500            DELIMITED BY SIZE INTO SSA1                                   
455600     MOVE 'WLKATH24 ' TO SSA2                                             
455700     MOVE '  ' TO GODK-STATUSKODER                                        
455800     CALL CBLTDLI USING ISRT AVS3-PCB IO-AREA-1 SSA1 SSA2                 
455900     MOVE AVS3-STATUS-CODE TO STATUS-WS                                   
456000     PERFORM IMS-STATUSKONTROLL                                           
456100     .                                                                    
456200     EJECT                                                                
456300 IMS-STATUSKONTROLL SECTION.                                              
456400     SET STATUS-IX TO 1                                                   
456500     SEARCH GODK-STATUS                                                   
456600       AT END                                                             
456700         CALL FELLOG                                                      
456800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
456900     END-SEARCH                                                           
457000     .                                                                    
