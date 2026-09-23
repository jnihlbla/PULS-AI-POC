000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1051300.                                                
000300 AUTHOR.         SUSANNE ENEGARD.                                         
000400 DATE-WRITTEN.   JANUARI 1985.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        PROGRAMMETS FRÅGEDELEN HÄMTAR INFORMATION OM NOTERING            
000900*                    OCH  EV. HÄNVISNING TILL ANNAT GRP/AVS/RAD           
001000*                    SAMT EV. REFERENSER FRÅN ANDRA GRP/AVS/RAD.          
001010*                                                                         
001020*                    Visar tabell över de pubkoder som refererande        
001030*                    rader har till detta avsnittshuvud (rad 0-14)        
001040*                    Om fler referenser finns med lika pubkod,            
001050*                    mot samma rad, visas bara en.                        
001070*                    En '*' framför pubkoden visar att man för            
001080*                    tillfället inte ser den pubkodens GRP/AVS/RAD        
001090*                    i tabellen p.g.a. vald RADPUB i nyckeln.             
001100*                                                                         
001200*        PROGRAMMETS UPPDATERINGSDEL NYREGISTRERAR OCH ÄNDRAR             
001300*                    NOTERINGAR.                                          
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W1T513                                              
001700*        MID:         W1I51301                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W1O51301                                            
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002602*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002603 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002604                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(8)    VALUE 'W1051300'.            
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000 77  OCH                         PIC X       VALUE '&'.                   
003100 77  ELLER                       PIC X       VALUE '!'.                   
003200 77  INDX                        PIC S9(9)   VALUE +1   COMP SYNC.        
003300 77  KOL                         PIC S9(9)   VALUE +1   COMP SYNC.        
003400 77  RAD                         PIC S9(9)   VALUE +1   COMP SYNC.        
003401 77  KC                          PIC S9(9)   VALUE +1   COMP SYNC.        
003410 77  RC                          PIC S9(9)   VALUE +1   COMP SYNC.        
003500 77  SPRAAK-IX                   PIC S9(9)   VALUE +1   COMP SYNC.        
003510 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
003600 77  INDATA-FEL                  PIC X       VALUE 'N'.                   
003610 77  SENASTE-RAD                 PIC S9(4)   VALUE +0.                    
003620 77  PUB-FINNS                   PIC X       VALUE 'N'.                   
003700                                                                          
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004001     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004002                                                                          
004010     EJECT                                                                
004020*    --- PARAMETRAR TILL SUBPROGRAM WDATAREA                              
004030*01  -COPY WDATAREA                                                       
004100                                                                          
004200 01  CURRDAT                      PIC X(8).                               
004201 01  FILLER     REDEFINES CURRDAT.                                        
004202     03 DAGENS-AAR                PIC 9(4).                               
004203 01  FILLER     REDEFINES CURRDAT.                                        
004204     03 DAGENS-SEKEL              PIC 9(2).                               
004205     03 DAGENS-DATUM              PIC 9(6).                               
004220                                                                          
004300 01  SPAR-IDCATRAD               PIC 9(4)   VALUE ZERO.                   
004400                                                                          
004500 01  IDCATNR-WS                  PIC X(5).                                
004600 01  FILLER          REDEFINES IDCATNR-WS.                                
004700     03  KEY-IDCATNR             PIC 9(5).                                
004800                                                                          
004900 01  IDCATGRP-WS                 PIC X(2).                                
005000 01  FILLER          REDEFINES IDCATGRP-WS.                               
005100     03  KEY-IDCATGRP            PIC 9(2).                                
005200                                                                          
005300 01  IDCATAVS-WS                 PIC X(4).                                
005400 01  FILLER          REDEFINES IDCATAVS-WS.                               
005500     03  KEY-IDCATAVS            PIC 9(4).                                
005600                                                                          
005700 01  IDCATRAD-WS                 PIC X(4).                                
005800 01  FILLER          REDEFINES IDCATRAD-WS.                               
005900     03  KEY-IDCATRAD            PIC 9(4).                                
006000                                                                          
006100 01  KEY-KDCATPUB                PIC X(6).                                
006110 01  WS-KDCATPUB-R-AVV           PIC X(3)    VALUE SPACE.                 
006120 01  WS-KDCATPUB-AAAAVV          PIC X(6)    VALUE SPACE.                 
006140 01  WS-GILTIGA-AAR.                                                      
006150   03 WS-TIAAAA                  PIC 9(4)    VALUE ZERO                   
006160                                 OCCURS 4.                                
006200     EJECT                                                                
006300* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
006400 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-T-DLI'.        
006500 01  NYCKLAR-TILL-DLI.                                                    
006600   03  W-WDN501KY-X.                                                      
006700     05  W-IDCATNR               PIC 9(5)   VALUE ZERO.                   
006800     05  W-IDCATGRP              PIC 9(2)   VALUE ZERO.                   
006900     05  W-IDCATAVS              PIC 9(4)   VALUE ZERO.                   
007000                                                                          
007100   03  W-WDN512KY-X.                                                      
007200     05  W-IDCATRAD-X.                                                    
007300       07  W-IDCATRAD            PIC 9(4)   VALUE ZERO.                   
007400     05  W-KDCATPUB-X            PIC X(6)   VALUE LOW-VALUE.              
007500                                                                          
007600   03  W-KDCATPUF-MIN            PIC X(3)   VALUE LOW-VALUE.              
007700   03  W-KDCATPUF-MAX            PIC X(3)   VALUE HIGH-VALUE.             
007800                                                                          
007900   03  W-IDSKYLT-X.                                                       
008000     05  W-IDSKYLT               PIC X(3)    VALUE SPACE.                 
008100                                                                          
008200   03  W-IDSEGMNR-X.                                                      
008300     05  W-IDSEGMNR              PIC S9      VALUE ZERO  COMP-3.          
008310                                                                          
008320   03 W-WDN5G1KY-X.                                                       
008330     05 W-WDN5G1KY-HAEN.                                                  
008340*        --- Hänvisat avsnitts RAD-ADRESS                                 
008341        07 W-WDN5GSEQ-01-X.                                               
008350          09 W-IDCATNR-GSEQ     PIC 9(5)   VALUE ZERO.                    
008360          09 W-IDCATGRP-GSEQ    PIC 9(2)   VALUE ZERO.                    
008370          09 W-IDCATAVS-GSEQ    PIC 9(4)   VALUE ZERO.                    
008371        07 W-WDN5GSEQ-12-X.                                               
008380          09 W-IDCATRAD-GSEQ    PIC 9(4)   VALUE ZERO.                    
008390          09 W-KDCATPUB-GSEQ    PIC X(6)   VALUE SPACE.                   
008391     05 W-IDWDN512-REF-X.                                                 
008392*        --- Hänvisande avsnittets RAD-ADRESS                             
008393*        --- FIELD NAME 'IDCATRKY' i det fysiska DBD:t WDN5G              
008394         07 W-IDCATNR-GSEQ-REF   PIC 9(5)   VALUE ZERO.                   
008395         07 W-IDCATGRP-GSEQ-REF  PIC 9(2)   VALUE ZERO.                   
008396         07 W-IDCATAVS-GSEQ-REF  PIC 9(4)   VALUE ZERO.                   
008397         07 W-IDCATRAD-GSEQ-REF  PIC 9(4)   VALUE ZERO.                   
008398         07 W-KDCATPUB-GSEQ-REF  PIC X(6)   VALUE SPACE.                  
008399                                                                          
008400   03  W-IDCATRKY-LO             PIC X(21) VALUE LOW-VALUE.               
008401   03  W-IDCATRKY-HI             PIC X(21) VALUE HIGH-VALUE.              
008402                                                                          
008410     EJECT                                                                
008500* - - - - - - - - - - - - - - - - - - -  MEDDELANDEN                      
008600 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
008700 01  MEDDELANDEN.                                                         
008800     03 FILLER-1.                                                         
008900          05 FILLER              PIC X(40)                                
009000              VALUE '    NYCKEL EJ NUMERISK                  '.           
009100          05 FILLER              PIC X(40)                                
009200              VALUE '    KEY NOT NUMERIC                     '.           
009300     03 FILLER REDEFINES FILLER-1.                                        
009400          05 FEL-1   OCCURS 2    PIC X(40).                               
009500     03 FILLER-2.                                                         
009600          05 FILLER              PIC X(40)                                
009700              VALUE '    RAD FINNS EJ                        '.           
009800          05 FILLER              PIC X(40)                                
009900              VALUE '    LINE NOT FOUND                      '.           
010000     03 FILLER REDEFINES FILLER-2.                                        
010100          05 FEL-2   OCCURS 2    PIC X(40).                               
010200     03 FILLER-3.                                                         
010300          05 FILLER              PIC X(40)                                
010400              VALUE '    UPPLYSTA FÄLT FEL                   '.           
010500          05 FILLER              PIC X(40)                                
010600              VALUE '    HILIGHTED FIELDS WRONG              '.           
010700     03 FILLER REDEFINES FILLER-3.                                        
010800          05 FEL-3   OCCURS 2    PIC X(40).                               
010900     03 FILLER-4.                                                         
011000          05 FILLER              PIC X(40)                                
011100              VALUE '    NYCKEL FINNS EJ                     '.           
011200          05 FILLER              PIC X(40)                                
011300              VALUE '    KEY NOT FOUND                       '.           
011400     03 FILLER REDEFINES FILLER-4.                                        
011500          05 FEL-4   OCCURS 2    PIC X(40).                               
011600     03 FILLER-11.                                                        
011700          05 FILLER              PIC X(61)                                
011800              VALUE '    UPPDATERING GJORD                   '.           
011900          05 FILLER              PIC X(61)                                
012000              VALUE '    UPDATING DONE                       '.           
012100     03 FILLER REDEFINES FILLER-11.                                       
012200          05 MED-1   OCCURS 2    PIC X(61).                               
012300     03 FILLER-12.                                                        
012400          05 FILLER              PIC X(61)                                
012500              VALUE '    KATALOGNR SAKNAS                    '.           
012600          05 FILLER              PIC X(61)                                
012700              VALUE '    CATALOGUENO. NOT FOUND              '.           
012800     03 FILLER REDEFINES FILLER-12.                                       
012900          05 MED-2   OCCURS 2    PIC X(61).                               
013000     EJECT                                                                
013100* - - - - - - - - - - - - - - - - - - - - MID-AREA                        
013200 01  FILLER                      PIC X(16)   VALUE '1512-MID'.            
013300*01  -COPY W1I51201  -PRE 1512- .                                         
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE '1513-MID'.            
013600*01  -COPY W1I51301.                                                      
013700     EJECT                                                                
013800* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
013900 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
014000*01  -COPY WMSGAREA                                                       
014100     EJECT                                                                
014200*    03  POST  -COPY W1O51301 -RED MSG-AREA.                              
014300     EJECT                                                                
014400* - - - - - - - - - - - - - - - - - - -  MFS-AREA                         
014500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014600*01  -COPY WMFSAREA                                                       
014700     EJECT                                                                
014800* - - - - - - - - - - - - - - - - - - -  IMS-WS                           
014900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015000 01  IMS-WS.                                                              
015100*                        **** STATUS-KOD FRÅN IMS                         
015200   03  STATUS-WS                 PIC XX.                                  
015300     88  SEGMENT-FINNS                       VALUE '  '.                  
015400     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
015500                                                   'GB'.                  
015600     SKIP2                                                                
015700   03  GODK-STATUSKODER.                                                  
015800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015900     SKIP2                                                                
016000 01  SSA1                        PIC X(144).                              
016100 01  SSA2                        PIC X(64).                               
016200 01  SSA3                        PIC X(64).                               
016300     EJECT                                                                
016400*                            IMS FUNKTIONSKODER                           
016500*01    -COPY W0003                                                        
016600     EJECT                                                                
016700* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA                      
016800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016900 01  DLI-IO-AREA.                                                         
017000     03  IO-AREA                 PIC X(50)  VALUE SPACE.                  
017100     SKIP3                                                                
017200*    03  WLKATH01 -COPY WDN501   -RED IO-AREA.                            
017300     EJECT                                                                
017400*    03  WLKATH12 -COPY WDN512   -RED IO-AREA.                            
017500     EJECT                                                                
017600*    03  WLKATH24 -COPY WDN524   -RED IO-AREA.                            
017700     EJECT                                                                
017800*    03  WLKATH27 -COPY WDN527   -RED IO-AREA.                            
017900     EJECT                                                                
018110     03  FILLER                  PIC X(16)  VALUE 'AVSG-IO-AREA'.         
018200     03  AVSG-IO-AREA            PIC X(48)  VALUE SPACE.                  
018400*    03 WLKATS01 -COPY WDN5G1  -RED AVSG-IO-AREA.                         
018500                                                                          
018510*    --- Till denna area flyttas AVSG-IDWDN512 från AVSG-IO-AREA          
018531     03  AVSG-REF-IDWDN512.                                               
018540        09  AVSG-REF-IDCATNR       PIC 9(5).                              
018550        09  AVSG-REF-IDCATGRP      PIC 9(2).                              
018560        09  AVSG-REF-IDCATAVS      PIC 9(4).                              
018570        09  AVSG-REF-IDCATRAD      PIC 9(4).                              
018580        09  AVSG-REF-KDCATPUB-FOM  PIC X(6).                              
018590     EJECT                                                                
018600 LINKAGE SECTION.                                                         
018700*01  -COPY W0009     -PRE MSG-                                            
018800     EJECT                                                                
018900*01  -COPY W0008     -PRE AVS-                                            
019000     05  FILLER                 PIC X.                                    
019100                                                                          
019200*01  -COPY W0008     -PRE KATS-                                           
019300     05  FILLER                 PIC X.                                    
019400     EJECT                                                                
019500 PROCEDURE DIVISION USING MSG-PCB AVS-PCB  KATS-PCB.                      
019600 MAIN SECTION.                                                            
019700     ENTRY 'DLITCBL' USING MSG-PCB AVS-PCB KATS-PCB.                      
019800                                                                          
019900     PERFORM IMS-GET-MSG                                                  
020000                                                                          
020100     IF SEGMENT-FINNS                                                     
020200       PERFORM A-INIT                                                     
020300                                                                          
020400       IF (IDCATNR-WS NOT NUMERIC)                                        
020500       OR (IDCATGRP-WS NOT NUMERIC)                                       
020600       OR (IDCATAVS-WS NOT NUMERIC)                                       
020700       OR (IDCATRAD-WS NOT NUMERIC)                                       
020800         MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                           
020900         PERFORM E-VISA-BILD-IGEN                                         
021000       ELSE                                                               
021100         IF KEY-IDCATRAD  > 1  AND < 10                                   
021200         OR KEY-IDCATRAD  > 14 AND < 20                                   
021300           MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                         
021400           PERFORM E-VISA-BILD-IGEN                                       
021500         ELSE                                                             
021600           MOVE KEY-IDCATNR  TO W-IDCATNR                                 
021700           MOVE KEY-IDCATGRP TO W-IDCATGRP                                
021800           MOVE KEY-IDCATAVS TO W-IDCATAVS                                
021900           MOVE KEY-IDCATRAD TO W-IDCATRAD                                
022000           MOVE KEY-KDCATPUB TO W-KDCATPUB-X                              
022100                                                                          
022200           IF MFS-UPDATE                                                  
022300             PERFORM B-KOLLA-INDATA                                       
022400                                                                          
022500             PERFORM C-UPPDATERA                                          
022600             IF INDATA-FEL = JA                                           
022700               PERFORM E-VISA-BILD-IGEN                                   
022800               MOVE FEL-2(SPRAAK-IX) TO MOD-TEMFSFEL                      
022900             ELSE                                                         
023000               MOVE MFS-RENSA-FAELT   TO MOD-TENOTE                       
023100               MOVE MED-1 (SPRAAK-IX) TO                                  
023200                    MOD-TEMFSINF                                          
023300               PERFORM D-LAS-NOTERING                                     
023400             END-IF                                                       
023500           ELSE                                                           
023600             PERFORM D-LAS-NOTERING                                       
023700           END-IF                                                         
023800         END-IF                                                           
023900       END-IF                                                             
024000         MOVE LENGTH OF MOD-W1O51301 TO MSG-KVLL                          
024100         ADD               +4       TO MSG-KVLL                           
024200       PERFORM IMS-INSERT-MSG                                             
024300     END-IF                                                               
024400     MOVE ZERO TO RETURN-CODE                                             
024500     GOBACK                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 A-INIT SECTION.                                                          
024900     SKIP2                                                                
025000     IF MSG-DUBBLA-TRANSKODER                                             
025100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I51301                 
025200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
025300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025400                                                                          
025500       IF MFS-IDTRANS = '1513'                                            
025600         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
025700       ELSE                                                               
025800         MOVE SPACE TO MFS-KDTRTYP                                        
025910       END-IF                                                             
026000     ELSE                                                                 
026100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I51301                  
026200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026400     END-IF                                                               
026500                                                                          
026501     IF MFS-IDTRANS = '1513'                                              
026502        CONTINUE                                                          
026503     ELSE                                                                 
026510       IF MFS-IDTRANS NOT = 1511 AND 1512 AND 1514 AND 1515               
026520         MOVE SPACE TO MID-IDSKYLT-IN                                     
026530                       MID-KDCATPUB-R-FOM-IN                              
026540         MOVE ZERO  TO MID-IDCATNR-IN                                     
026550                       MID-IDCATGRP-IN                                    
026560                       MID-IDCATAVS-IN                                    
026570                       MID-IDCATRAD-IN                                    
026580       END-IF                                                             
026581     END-IF                                                               
026582                                                                          
026583     MOVE FUNCTION CURRENT-DATE TO CURRDAT                                
026584                                                                          
026585     COMPUTE WS-TIAAAA(1) = DAGENS-AAR - 1                                
026586     COMPUTE WS-TIAAAA(2) = DAGENS-AAR                                    
026587     COMPUTE WS-TIAAAA(3) = DAGENS-AAR + 1                                
026588     COMPUTE WS-TIAAAA(4) = DAGENS-AAR + 2                                
026590                                                                          
026600     IF MID-IDCATNR-IN = ALL '+'                                          
026700       MOVE MID-IDCATNR-UT TO IDCATNR-WS                                  
026800       INSPECT IDCATNR-WS REPLACING LEADING SPACE BY ZERO                 
026900     ELSE                                                                 
027000       MOVE MID-IDCATNR-IN TO IDCATNR-WS                                  
027200     END-IF                                                               
027300                                                                          
027400     IF MID-IDCATGRP-IN = ALL '+'                                         
027500       MOVE MID-IDCATGRP-UT TO IDCATGRP-WS                                
027600       INSPECT IDCATGRP-WS REPLACING LEADING SPACE BY ZERO                
027700     ELSE                                                                 
027800       MOVE MID-IDCATGRP-IN TO IDCATGRP-WS                                
028000     END-IF                                                               
028100                                                                          
028200     IF MID-IDCATAVS-IN = ALL '+'                                         
028300       MOVE MID-IDCATAVS-UT TO IDCATAVS-WS                                
028400       INSPECT IDCATAVS-WS REPLACING LEADING SPACE BY ZERO                
028500     ELSE                                                                 
028600       MOVE MID-IDCATAVS-IN TO IDCATAVS-WS                                
028800     END-IF                                                               
028900                                                                          
029000     IF MID-IDCATRAD-IN = ALL '+'                                         
029100       MOVE MID-IDCATRAD-UT TO IDCATRAD-WS                                
029200       INSPECT IDCATRAD-WS REPLACING LEADING SPACE BY ZERO                
029300     ELSE                                                                 
029400       MOVE MID-IDCATRAD-IN TO IDCATRAD-WS                                
029600     END-IF                                                               
029700                                                                          
029800     IF MID-KDCATPUB-R-FOM-IN = ALL '+'                                   
029900       MOVE MID-KDCATPUB-R-FOM-UT                                         
029910                             TO WS-KDCATPUB-R-AVV                         
029920       PERFORM S50-Y2K-KDCATPUB-R                                         
029930       MOVE WS-KDCATPUB-AAAAVV                                            
029940                             TO KEY-KDCATPUB                              
030000     ELSE                                                                 
030100       MOVE MID-KDCATPUB-R-FOM-IN                                         
030110                             TO WS-KDCATPUB-R-AVV                         
030120       PERFORM S50-Y2K-KDCATPUB-R                                         
030130       MOVE WS-KDCATPUB-AAAAVV                                            
030140                             TO KEY-KDCATPUB                              
030300     END-IF                                                               
030400                                                                          
030500     IF MID-IDSKYLT-IN = ALL '+'                                          
030600       MOVE MID-IDSKYLT-UT TO W-IDSKYLT                                   
030700     ELSE                                                                 
030800       MOVE MID-IDSKYLT-IN TO W-IDSKYLT                                   
030900     END-IF                                                               
030910                                                                          
030920     IF  MID-IDCATNR-IN        = ALL '+'                                  
030930     AND MID-IDCATGRP-IN       = ALL '+'                                  
030940     AND MID-IDCATAVS-IN       = ALL '+'                                  
030950     AND MID-IDCATRAD-IN       = ALL '+'                                  
030960     AND MID-KDCATPUB-R-FOM-IN = ALL '+'                                  
030970     AND MID-IDSKYLT-IN        = ALL '+'                                  
030971       CONTINUE                                                           
030972     ELSE                                                                 
030973       MOVE SPACE TO MFS-KDTRTYP                                          
030974     END-IF                                                               
030980                                                                          
031000     MOVE LOW-VALUE TO MSG-AREA                                           
031100     MOVE 'W1O51301' TO MFS-IDMOD                                         
031200     MOVE '1513' TO MOD-IDTRANS                                           
031300                                                                          
031400     IF ENGLISH-TEXT                                                      
031500       MOVE +2 TO SPRAAK-IX                                               
031600     ELSE                                                                 
031700       MOVE +1 TO SPRAAK-IX                                               
031800     END-IF                                                               
031900     MOVE IDCATNR-WS TO MOD-IDCATNR-UT                                    
032000     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
032100                                                                          
032200     MOVE IDCATGRP-WS TO MOD-IDCATGRP-UT                                  
032300     INSPECT MOD-IDCATGRP-UT REPLACING LEADING ZERO BY SPACE              
032400                                                                          
032500     MOVE IDCATAVS-WS TO MOD-IDCATAVS-UT                                  
032600     INSPECT MOD-IDCATAVS-UT REPLACING LEADING ZERO BY SPACE              
032700                                                                          
032710     INSPECT W-IDSKYLT REPLACING LEADING '   ' BY 'S  '                   
032800     MOVE W-IDSKYLT   TO MOD-IDSKYLT-UT                                   
032801                                                                          
032810     MOVE IDCATRAD-WS TO MOD-IDCATRAD-UT                                  
032900     IF MOD-IDCATRAD-UT = ZERO                                            
033000       MOVE '   0' TO MOD-IDCATRAD-UT                                     
033100     ELSE                                                                 
033200       INSPECT MOD-IDCATRAD-UT REPLACING LEADING ZERO BY SPACE            
033300     END-IF                                                               
033400                                                                          
033500     IF KEY-KDCATPUB = ALL '+'                                            
033600       MOVE SPACE TO KEY-KDCATPUB                                         
033700     END-IF                                                               
033840     MOVE KEY-KDCATPUB (4:3) TO MOD-KDCATPUB-R-FOM-UT                     
033900     INSPECT KEY-KDCATPUB REPLACING ALL SPACE BY LOW-VALUE                
034000                                                                          
034100     PERFORM AA-FIXA-HOPPNYCKLAR                                          
034200                                                                          
034300     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
034400                             MOD-IDCATGRP-IN                              
034500                             MOD-IDCATAVS-IN                              
034600                             MOD-IDSKYLT-IN                               
034700                             MOD-IDCATRAD-IN                              
034800                             MOD-KDCATPUB-R-FOM-IN                        
034900                             MOD-TEMFSFEL                                 
035000                             MOD-TEMFSINF                                 
035010                                                                          
035310     .                                                                    
035400     EJECT                                                                
035500 AA-FIXA-HOPPNYCKLAR SECTION.                                             
035600     SKIP2                                                                
035700     IF MFS-IDTRANS = '1512'                                              
035800       MOVE MID-W1I51301 TO 1512-MID-W1I51201                             
035900       IF 1512-MID-KDCATPUB-R-MIN-IN = ALL '+'                            
036000         MOVE 1512-MID-KDCATPUB-R-MIN-UT TO MOD-KDCATPUB-R-MIN            
036100       ELSE                                                               
036200         MOVE 1512-MID-KDCATPUB-R-MIN-IN TO MOD-KDCATPUB-R-MIN            
036300       END-IF                                                             
036400       IF 1512-MID-KDCATPUB-R-MAX-IN = ALL '+'                            
036500         MOVE 1512-MID-KDCATPUB-R-MAX-UT TO MOD-KDCATPUB-R-MAX            
036600       ELSE                                                               
036700         MOVE 1512-MID-KDCATPUB-R-MAX-IN TO MOD-KDCATPUB-R-MAX            
036800       END-IF                                                             
036900     ELSE                                                                 
037000       IF MFS-IDTRANS = '1513'                                            
037100         MOVE MID-KDCATPUB-R-MIN TO MOD-KDCATPUB-R-MIN                    
037200         MOVE MID-KDCATPUB-R-MAX TO MOD-KDCATPUB-R-MAX                    
037300       ELSE                                                               
037400         MOVE LOW-VALUE TO MOD-KDCATPUB-R-MIN                             
037500         MOVE HIGH-VALUE TO MOD-KDCATPUB-R-MAX                            
037600       END-IF                                                             
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 B-KOLLA-INDATA SECTION.                                                  
038100     SKIP2                                                                
038200     MOVE NEJ TO INDATA-FEL                                               
038300                                                                          
038400     IF MID-TENOTE NOT = ALL '+'                                          
038500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-ATTR                       
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 C-UPPDATERA SECTION.                                                     
039000     SKIP2                                                                
039100     IF INDATA-FEL = NEJ                                                  
039200       PERFORM IMS-GHU-AVS-RAD                                            
039300       IF SEGMENT-SAKNAS                                                  
039400         MOVE JA TO INDATA-FEL                                            
039500       ELSE                                                               
039600         MOVE DAGENS-DATUM      TO RAD-TIUPPDAT                           
039700         MOVE MSG-SIGNON-USERID TO RAD-IDUSER                             
039800         MOVE      'Ä'          TO RAD-KDRADST                            
039900         PERFORM IMS-REPL-AVS                                             
040000       END-IF                                                             
040100     END-IF                                                               
040200     IF INDATA-FEL = NEJ                                                  
040300       MOVE +1 TO W-IDSEGMNR                                              
040400       PERFORM IMS-GHNP-AVS-NOT                                           
040500       IF SEGMENT-FINNS                                                   
040600         IF MID-TENOTE = ALL SPACE                                        
040700           PERFORM IMS-DLET-AVS                                           
040800         ELSE                                                             
040900           MOVE MID-TENOTE TO NOT-TENOTE                                  
041000           PERFORM IMS-REPL-AVS                                           
041100         END-IF                                                           
041200       ELSE                                                               
041300         IF MID-TENOTE NOT = ALL SPACE                                    
041400           MOVE MID-TENOTE TO NOT-TENOTE                                  
041500           MOVE '1'        TO NOT-IDSEGMNR                                
041600           PERFORM IMS-ISRT-AVS-NOT                                       
041700         END-IF                                                           
041800       END-IF                                                             
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 D-LAS-NOTERING SECTION.                                                  
042300     SKIP2                                                                
042400     PERFORM IMS-GU-AVS                                                   
042500                                                                          
042600     IF SEGMENT-FINNS                                                     
042700*      Hämta först information om raderna 0 - 14                          
042800       PERFORM DA-LAS-AVS-PATH-REF-RAD-0-14                               
042900                                                                          
043000       PERFORM IMS-GU-AVS-RAD                                             
043100       IF SEGMENT-FINNS                                                   
043200         MOVE KEY-IDCATRAD TO W-IDCATRAD                                  
043300         MOVE KEY-KDCATPUB TO W-KDCATPUB-X                                
043400*         Återställer till den sökta IDCATRAD efter läsn av 0 - 14        
043500         PERFORM IMS-GU-AVS-RAD                                           
043600         PERFORM DB-LAS-AVS-NOT-IDCATRAD                                  
043700         PERFORM DC-LAS-AVS-REF-IDCATRAD                                  
043800       ELSE                                                               
043900         MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE                             
044000         MOVE FEL-2 (SPRAAK-IX) TO MOD-TEMFSINF                           
044100       END-IF                                                             
044200     ELSE                                                                 
044300       MOVE MFS-RENSA-FAELT TO MOD-TENOTE                                 
044400       MOVE FEL-4 (SPRAAK-IX) TO MOD-TEMFSINF                             
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 DA-LAS-AVS-PATH-REF-RAD-0-14 SECTION.                                    
044900     SKIP2                                                                
045000*    Nollställ referenskolumnen                                           
045100     MOVE +1 TO KOL                                                       
045200     MOVE +1 TO RAD                                                       
045300     PERFORM UNTIL KOL > +5                                               
045400       PERFORM UNTIL RAD > +9                                             
045500         MOVE '-' TO MOD-REF-KDHAEN-KOL(KOL, RAD)                         
045600         MOVE '---' TO MOD-REF-KDCATPUB-R-FOM-KOL(KOL, RAD)               
045700         ADD  +1  TO RAD                                                  
045800       END-PERFORM                                                        
045900       ADD  +1 TO KOL                                                     
046000       MOVE +1 TO RAD                                                     
046100     END-PERFORM                                                          
046110                                                                          
046200*    --- OK, kör raderna                                                  
046300*    --- Läs WLKATS, seq-ix för hänv. (WDN5G)                             
046400     MOVE +1 TO RAD, SENASTE-RAD                                          
046401                                                                          
046410*    --- Kolla om det finns en HAEN-REF till avsnittet som helhet         
046430     MOVE W-WDN501KY-X  TO W-WDN5GSEQ-01-X                                
046431     MOVE W-WDN512KY-X  TO W-WDN5GSEQ-12-X                                
046440     MOVE +0            TO W-IDCATRAD-GSEQ                                
046450*    --- Läs med "blank" KDCATPUB för rad NOLL                            
046460     MOVE LOW-VALUE     TO W-KDCATPUB-GSEQ                                
046500     PERFORM IMS-GU-AVSG-HAEN                                             
046501                                                                          
046510     MOVE '-' TO MOD-REF-KDHAEN-AVS                                       
046600     IF SEGMENT-FINNS                                                     
047110       IF AVSG-KDCATPUB-FOM = KEY-KDCATPUB                                
047200         MOVE 'R' TO MOD-REF-KDHAEN-AVS                                   
047201*        --- indikerar att ref finns till efterfrågad pubkod              
047210       ELSE                                                               
047220         MOVE '*' TO MOD-REF-KDHAEN-AVS                                   
047221*        --- ref finns till annan pubkod än efterfrågad                   
047230       END-IF                                                             
047231     END-IF                                                               
047233                                                                          
047234*    --- Återställ till sökt KDCATPUB                                     
047235     MOVE KEY-KDCATPUB TO W-KDCATPUB-GSEQ                                 
047236*    --- Kolla sedan rad 10 och öka med 1 tills man får träff             
047237*    --- upp till rad 14 (= Kolumn A - E)                                 
047238                                                                          
047240     MOVE +10          TO W-IDCATRAD-GSEQ                                 
047241     PERFORM IMS-GU-AVSG-HAEN                                             
047242                                                                          
047250     PERFORM UNTIL SEGMENT-FINNS OR W-IDCATRAD-GSEQ > +14                 
047252       ADD +1 TO W-IDCATRAD-GSEQ                                          
047253       PERFORM IMS-GU-AVSG-HAEN                                           
047260     END-PERFORM                                                          
047270                                                                          
047430     IF SEGMENT-FINNS                                                     
047440*      --- Nu har man träff på EN kolumn som har en HAEN-REF              
047600       PERFORM UNTIL W-IDCATRAD-GSEQ > 14                                 
047601*        --- Snurra på, upp till och med rad 14 (Kol E)                   
047602         PERFORM UNTIL SEGMENT-SAKNAS                                     
047603           MOVE AVSG-IDWDN512  TO AVSG-REF-IDWDN512                       
047604*          --- Snurra på, så länge det finns REF-segment                  
047610           IF SENASTE-RAD NOT = W-IDCATRAD-GSEQ                           
047620             MOVE +1 TO RAD                                               
047630             MOVE W-IDCATRAD-GSEQ TO SENASTE-RAD                          
047640           END-IF                                                         
047710           EVALUATE W-IDCATRAD-GSEQ                                       
047800*   A-KOLUMNEN                                                            
047900             WHEN 10                                                      
048000               MOVE +1 TO KOL                                             
048001               PERFORM DAA-KOLLA-OM-PUB-SKRIVEN                           
048002               IF PUB-FINNS = NEJ                                         
048010                 IF AVSG-REF-KDCATPUB-FOM = KEY-KDCATPUB                  
048011                   MOVE 'R' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
048020                 ELSE                                                     
048021*                  --- ref finns till annan pubkod än efterfrågad         
048022                   MOVE '*' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
048030                 END-IF                                                   
048100                 MOVE AVSG-REF-KDCATPUB-FOM (4:3)                         
048200                          TO MOD-REF-KDCATPUB-R-FOM-KOL(KOL, RAD)         
048300                 ADD +1 TO RAD                                            
048400               END-IF                                                     
048410*   B-KOLUMNEN                                                            
048500             WHEN 11                                                      
048700               MOVE +2 TO KOL                                             
048800               PERFORM DAA-KOLLA-OM-PUB-SKRIVEN                           
048801               IF PUB-FINNS = NEJ                                         
048810                 IF AVSG-REF-KDCATPUB-FOM = KEY-KDCATPUB                  
048900                   MOVE 'R' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
048910                 ELSE                                                     
048911                   MOVE '*' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
048920                 END-IF                                                   
049000                 MOVE AVSG-REF-KDCATPUB-FOM (4:3)                         
049100                          TO MOD-REF-KDCATPUB-R-FOM-KOL(KOL, RAD)         
049300                 ADD +1 TO RAD                                            
049400               END-IF                                                     
049500*   C-KOLUMNEN                                                            
049510             WHEN 12                                                      
049600               MOVE +3 TO KOL                                             
049700               PERFORM DAA-KOLLA-OM-PUB-SKRIVEN                           
049701               IF PUB-FINNS = NEJ                                         
049710                 IF AVSG-REF-KDCATPUB-FOM = KEY-KDCATPUB                  
049720                   MOVE 'R' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
049730                 ELSE                                                     
049740                   MOVE '*' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
049750                 END-IF                                                   
049900                 MOVE AVSG-REF-KDCATPUB-FOM (4:3)                         
050000                          TO MOD-REF-KDCATPUB-R-FOM-KOL(KOL, RAD)         
050100                 ADD +1 TO RAD                                            
050200               END-IF                                                     
050300*   D-KOLUMNEN                                                            
050310             WHEN 13                                                      
050400               MOVE +4 TO KOL                                             
050500               PERFORM DAA-KOLLA-OM-PUB-SKRIVEN                           
050501               IF PUB-FINNS = NEJ                                         
050510                 IF AVSG-REF-KDCATPUB-FOM = KEY-KDCATPUB                  
050520                   MOVE 'R' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
050530                 ELSE                                                     
050540                   MOVE '*' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
050550                 END-IF                                                   
050700                 MOVE AVSG-REF-KDCATPUB-FOM (4:3)                         
050800                          TO MOD-REF-KDCATPUB-R-FOM-KOL(KOL, RAD)         
050900                 ADD +1 TO RAD                                            
051000               END-IF                                                     
051100*   E-KOLUMNEN                                                            
051110             WHEN 14                                                      
051200               MOVE +5 TO KOL                                             
051300               PERFORM DAA-KOLLA-OM-PUB-SKRIVEN                           
051301               IF PUB-FINNS = NEJ                                         
051310                 IF AVSG-REF-KDCATPUB-FOM = KEY-KDCATPUB                  
051320                   MOVE 'R' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
051330                 ELSE                                                     
051340                   MOVE '*' TO MOD-REF-KDHAEN-KOL(KOL, RAD)               
051350                 END-IF                                                   
051500                 MOVE AVSG-REF-KDCATPUB-FOM (4:3)                         
051600                          TO MOD-REF-KDCATPUB-R-FOM-KOL(KOL, RAD)         
051700                 ADD +1 TO RAD                                            
051710               END-IF                                                     
051720                                                                          
051800             WHEN OTHER                                                   
051900               CONTINUE                                                   
052000           END-EVALUATE                                                   
052001*          --- Läser vidare på samma IDCATRAD, för fler REF:ar            
052002           PERFORM IMS-GN-AVSG-HAEN                                       
052010         END-PERFORM                                                      
052011                                                                          
052020         ADD +1 TO W-IDCATRAD-GSEQ                                        
052021*        --- Nästa IDCATRAD (KOLUMN)                                      
052030         PERFORM IMS-GU-AVSG-HAEN                                         
052200       END-PERFORM                                                        
052300     END-IF                                                               
052400     .                                                                    
052500     EJECT                                                                
052501 DAA-KOLLA-OM-PUB-SKRIVEN SECTION.                                        
052502     SKIP2                                                                
052503*    Här kollas om referensens PUB redan är skriven i resp kolumn         
052504*    vilken bara har plats för 9 rader.                                   
052505*    Man behöver bara veta vilka unika pubkoder som refererar             
052506*    till raden.                                                          
052507*    Adr. till alla ref. fås vid angivande av varje PUB i nyckeln.        
052508                                                                          
052509*                       KC = KOLUMN-INDEX (CHECK)                         
052510*                       RC = RAD-INDEX (CHECK)                            
052512     MOVE NEJ TO PUB-FINNS                                                
052513     MOVE +1  TO RC                                                       
052514     MOVE KOL TO KC                                                       
052515                                                                          
052516     PERFORM UNTIL RC >= RAD                                              
052517*      --- raden RAD är ännu inte skriven.                                
052518       IF AVSG-REF-KDCATPUB-FOM (4:3)                                     
052519                            =  MOD-REF-KDCATPUB-R-FOM-KOL(KC, RC)         
052520         MOVE JA  TO PUB-FINNS                                            
052521         MOVE RAD TO RC                                                   
052522       END-IF                                                             
052523       ADD +1 TO RC                                                       
052524     END-PERFORM                                                          
052525     .                                                                    
052530     EJECT                                                                
052600 DB-LAS-AVS-NOT-IDCATRAD SECTION.                                         
052700     SKIP2                                                                
052800     MOVE +1 TO W-IDSEGMNR                                                
053000     PERFORM IMS-GNP-AVS-NOT                                              
053100     IF SEGMENT-FINNS                                                     
053200       MOVE NOT-TENOTE TO MOD-TENOTE                                      
053300     ELSE                                                                 
053400       MOVE SPACE      TO MOD-TENOTE                                      
053500     END-IF                                                               
053600     MOVE +2 TO W-IDSEGMNR                                                
053700     PERFORM IMS-GNP-AVS-NOT                                              
053800                                                                          
053900     IF SEGMENT-FINNS                                                     
054000       MOVE NOT-IDCATGRP TO MOD-IDCATGRP                                  
054100       MOVE NOT-IDCATAVS TO MOD-IDCATAVS                                  
054200       MOVE NOT-IDCATRAD TO MOD-IDCATRAD                                  
054300       MOVE NOT-KDCATPUB-FOM (4:3) TO MOD-KDCATPUB-R-FOM                  
054400       MOVE NOT-KDCATPUB-TOM (4:3) TO MOD-KDCATPUB-R-TOM                  
054500       MOVE '*'          TO MOD-KDHAEN                                    
054600     ELSE                                                                 
054700       PERFORM IMS-GNP-AVS-HAEN                                           
054800       IF SEGMENT-FINNS                                                   
054900         MOVE HAEN-IDCATGRP TO MOD-IDCATGRP                               
055000         MOVE HAEN-IDCATAVS TO MOD-IDCATAVS                               
055100         MOVE HAEN-IDCATRAD TO MOD-IDCATRAD                               
055200         MOVE HAEN-KDCATPUB-FOM (4:3) TO MOD-KDCATPUB-R-FOM               
055300         MOVE SPACE                   TO MOD-KDCATPUB-R-TOM               
055400         MOVE HAEN-KDHAEN   TO MOD-KDHAEN                                 
055500       ELSE                                                               
055600         MOVE ZERO          TO MOD-IDCATGRP                               
055700                               MOD-IDCATAVS                               
055800                               MOD-IDCATRAD                               
055900         MOVE SPACE         TO MOD-KDCATPUB-R-FOM                         
056000                               MOD-KDCATPUB-R-TOM                         
056100                               MOD-KDHAEN                                 
056200       END-IF                                                             
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 DC-LAS-AVS-REF-IDCATRAD SECTION.                                         
056700     SKIP2                                                                
056800     MOVE W-WDN512KY-X  TO W-WDN5GSEQ-12-X                                
056900     PERFORM IMS-GU-AVSG-HAEN                                             
057000     MOVE +1 TO RAD                                                       
057100     PERFORM UNTIL SEGMENT-SAKNAS                                         
057200     OR RAD > +28                                                         
057210       MOVE AVSG-IDWDN512     TO AVSG-REF-IDWDN512                        
057300       MOVE AVSG-REF-IDCATGRP TO MOD-REF-IDCATGRP(RAD)                    
057400       MOVE AVSG-REF-IDCATAVS TO MOD-REF-IDCATAVS(RAD)                    
057500       MOVE AVSG-REF-IDCATRAD TO MOD-REF-IDCATRAD(RAD)                    
057600       MOVE AVSG-REF-KDCATPUB-FOM (4:3)                                   
057610                              TO MOD-REF-KDCATPUB-R-FOM(RAD)              
057700       MOVE AVSG-KDHAEN       TO MOD-REF-KDHAEN  (RAD)                    
057800       ADD +1 TO RAD                                                      
057900       PERFORM IMS-GN-AVSG-HAEN                                           
058000     END-PERFORM                                                          
058100                                                                          
058200     IF RAD > +28                                                         
058300       CONTINUE                                                           
058400     ELSE                                                                 
058500       PERFORM UNTIL RAD > +28                                            
058600         MOVE '0000'         TO MOD-REF-IDCATGRP (RAD)                    
058700                                MOD-REF-IDCATAVS (RAD)                    
058800                                MOD-REF-IDCATRAD (RAD)                    
058900         MOVE SPACE          TO MOD-REF-KDCATPUB-R-FOM(RAD)               
059000                                MOD-REF-KDHAEN   (RAD)                    
059100         ADD +1 TO RAD                                                    
059200       END-PERFORM                                                        
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059520*                                                                         
059530* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
059540* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
059550*                                                                         
059560*    -COPY W150Y2K1                                                       
059580     EJECT                                                                
059600 E-VISA-BILD-IGEN SECTION.                                                
059700     SKIP2                                                                
059800     MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE                                 
059900                               MOD-IDCATGRP                               
060000                               MOD-IDCATAVS                               
060100                               MOD-IDCATRAD                               
060200                               MOD-KDCATPUB-R-FOM                         
060300                               MOD-KDCATPUB-R-TOM                         
060400                               MOD-KDHAEN                                 
060500     .                                                                    
060600     EJECT                                                                
060700* IMS SEKTIONER                                                           
060800     SKIP1                                                                
060900 IMS-GET-MSG SECTION.                                                     
061000     MOVE '  QC' TO GODK-STATUSKODER                                      
061100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500     SKIP3                                                                
061600 IMS-INSERT-MSG SECTION.                                                  
061700     IF ENGLISH-TEXT                                                      
061800       MOVE 'N' TO MFS-KDHUVOMR                                           
061900     END-IF                                                               
062000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
062100     MOVE SPACE TO GODK-STATUSKODER                                       
062200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
062300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062400     PERFORM IMS-STATUSKONTROLL                                           
062500     .                                                                    
062600     EJECT                                                                
062700 IMS-GU-AVS SECTION.                                                      
062800     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
062900            DELIMITED BY SIZE INTO SSA1                                   
063000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
063100     CALL CBLTDLI USING GU AVS-PCB IO-AREA SSA1                           
063200     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
063300     PERFORM IMS-STATUSKONTROLL                                           
063400     .                                                                    
063500     EJECT                                                                
063600 IMS-GU-AVS-RAD SECTION.                                                  
063700     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
063800            DELIMITED BY SIZE INTO SSA1                                   
063900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
064000            DELIMITED BY SIZE INTO SSA2                                   
064100     MOVE '  GE' TO GODK-STATUSKODER                                      
064200     CALL CBLTDLI USING GU AVS-PCB IO-AREA SSA1 SSA2                      
064300     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
064400     PERFORM IMS-STATUSKONTROLL                                           
064500     .                                                                    
064600     SKIP2                                                                
064700 IMS-GHU-AVS-RAD SECTION.                                                 
064800     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
064900            DELIMITED BY SIZE INTO SSA1                                   
065000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
065100            DELIMITED BY SIZE INTO SSA2                                   
065200     MOVE '  GE' TO GODK-STATUSKODER                                      
065300     CALL CBLTDLI USING GHU AVS-PCB IO-AREA SSA1 SSA2                     
065400     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700     SKIP2                                                                
065800 IMS-GNP-AVS-NOT SECTION.                                                 
065900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
066000            DELIMITED BY SIZE INTO SSA1                                   
066100     STRING 'WLKATH24(IDSEGMNR =' W-IDSEGMNR-X ')'                        
066200            DELIMITED BY SIZE INTO SSA2                                   
066300     MOVE '  GE' TO GODK-STATUSKODER                                      
066400     CALL CBLTDLI USING GNP AVS-PCB IO-AREA SSA1 SSA2                     
066500     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
066600     PERFORM IMS-STATUSKONTROLL                                           
066700     .                                                                    
066800     SKIP2                                                                
066900 IMS-GHNP-AVS-NOT SECTION.                                                
067000     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
067100            DELIMITED BY SIZE INTO SSA1                                   
067200     STRING 'WLKATH24(IDSEGMNR =' W-IDSEGMNR-X ')'                        
067300            DELIMITED BY SIZE INTO SSA2                                   
067400     MOVE '  GE' TO GODK-STATUSKODER                                      
067500     CALL CBLTDLI USING GHNP AVS-PCB IO-AREA SSA1 SSA2                    
067600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
067700     PERFORM IMS-STATUSKONTROLL                                           
067800     .                                                                    
067900     SKIP2                                                                
068000 IMS-ISRT-AVS-NOT SECTION.                                                
068100     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
068200            DELIMITED BY SIZE INTO SSA1                                   
068300     MOVE 'WLKATH24 ' TO SSA2                                             
068400     MOVE '  ' TO GODK-STATUSKODER                                        
068500     CALL CBLTDLI USING ISRT AVS-PCB IO-AREA SSA1 SSA2                    
068600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
068700     PERFORM IMS-STATUSKONTROLL                                           
068800     .                                                                    
068900     SKIP2                                                                
069000 IMS-GNP-AVS-HAEN SECTION.                                                
069100     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
069200            DELIMITED BY SIZE INTO SSA1                                   
069300     MOVE 'WLKATH27 ' TO SSA2                                             
069400     MOVE '  GE' TO GODK-STATUSKODER                                      
069500     CALL CBLTDLI USING GNP AVS-PCB IO-AREA SSA1 SSA2                     
069600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     .                                                                    
069900     EJECT                                                                
069910 IMS-GU-AVSG-HAEN SECTION.                                                
069911     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5G1KY-HAEN                         
069913                                  W-IDCATRKY-LO                           
069914                 OCH 'WDN5G1KY<=' W-WDN5G1KY-HAEN                         
069916                                  W-IDCATRKY-HI ')'                       
069930            DELIMITED BY SIZE INTO SSA1                                   
069940     MOVE '  GE' TO GODK-STATUSKODER                                      
069950     CALL CBLTDLI USING GU KATS-PCB AVSG-IO-AREA SSA1                     
069960     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
069970     PERFORM IMS-STATUSKONTROLL                                           
069980     .                                                                    
069990     SKIP2                                                                
069991 IMS-GN-AVSG-HAEN SECTION.                                                
069992     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5G1KY-HAEN                         
069994                                  W-IDCATRKY-LO                           
069995                 OCH 'WDN5G1KY<=' W-WDN5G1KY-HAEN                         
069997                                  W-IDCATRKY-HI ')'                       
070000            DELIMITED BY SIZE INTO SSA1                                   
070001     MOVE '  GE' TO GODK-STATUSKODER                                      
070002     CALL CBLTDLI USING GN KATS-PCB AVSG-IO-AREA SSA1                     
070003     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
070004     PERFORM IMS-STATUSKONTROLL                                           
070010     .                                                                    
070100     EJECT                                                                
073600 IMS-DLET-AVS SECTION.                                                    
073700     MOVE '  ' TO GODK-STATUSKODER                                        
073800     CALL CBLTDLI USING DLET AVS-PCB IO-AREA                              
073900     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
074000     PERFORM IMS-STATUSKONTROLL                                           
074100     .                                                                    
074200     SKIP2                                                                
074300 IMS-REPL-AVS SECTION.                                                    
074400     MOVE '  ' TO GODK-STATUSKODER                                        
074500     CALL CBLTDLI USING REPL AVS-PCB IO-AREA                              
074600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
074700     PERFORM IMS-STATUSKONTROLL                                           
074800     .                                                                    
074900     EJECT                                                                
075000 IMS-STATUSKONTROLL SECTION.                                              
075100     SET STATUS-IX TO 1                                                   
075200     SEARCH GODK-STATUS                                                   
075300       AT END                                                             
075400         CALL FELLOG                                                      
075500     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
075600       CONTINUE                                                           
075700     END-SEARCH                                                           
075800     .                                                                    
