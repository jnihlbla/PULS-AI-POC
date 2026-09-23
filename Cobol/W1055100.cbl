000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1055100.                                                
000400 AUTHOR.         ODD OLSEN                                                
000500 DATE-WRITTEN.   OKTOBER 85.                                              
000510 DATE-COMPILED.                                                           
000600*                                                                         
000610*BASICLY ALTERED  CONNY EGHOLT                                            
000700*                 NOVEMBER 1996                                           
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        Programmet omnumrerar raderna på ett katalogavsnitt              
001100*        i reservdelskatalogen så att raderna får radnummer               
001200*        med 10-intervall.                                                
001300*                                                                         
001310*        Under omnumreringen så lägger programmet vid behov               
001320*        temporärt upp rader på radnummer 8000 och däröver.               
001330*                                                                         
001340*        Programmet omnumrerar max 80 rader åt gången för att             
001350*        sedan via en program-to-program-switch starta                    
001360*        programmet igen tills hela avsnittet är omnumrerat.              
001370*        detta för att programmet inte skall åka ut på tid.               
001380*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W1T551                                              
002800*        MID:         W1I55101                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W1O55101                                            
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W1055100'.            
003900 77    JA                        PIC X       VALUE 'J'.                   
004000 77    NEJ                       PIC X       VALUE 'N'.                   
004100 77    OCH                       PIC X       VALUE '&'.                   
004200 77    OMNUMRERA-KLAR            PIC X       VALUE 'N'.                   
004300 77    FL-8000-RADER             PIC X       VALUE 'N'.                   
004400 77    OMNUMRERA-8000-RADER      PIC X       VALUE 'N'.                   
004700 77    SPRAAK-IX                 PIC S9(9)   COMP SYNC VALUE ZERO.        
004800 01    FILLER                    PIC X(12)                                
004900                                 VALUE 'IMS-CALL-ID='.                    
005000 77    IMS-CALL-ID               PIC X(30).                               
005010 01    FILLER                    PIC X(12)                                
005020                                 VALUE 'HAEN-ADRESS='.                    
005030 77    IMS-HAEN-ID               PIC X(30).                               
005040 01    FILLER                    PIC X(12)                                
005050                                 VALUE ' REF-ADRESS='.                    
005060 77    IMS-REF-ID                PIC X(30).                               
005100     SKIP2                                                                
005200                                                                          
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005600                                                                          
005900 01  IDCATNR-WS                  PIC X(5)    VALUE SPACE.                 
006000 01  FILLER              REDEFINES IDCATNR-WS.                            
006100     03  KEY-IDCATNR             PIC 9(5).                                
006200 01  IDCATGRP-WS                 PIC X(2)    VALUE SPACE.                 
006300 01  FILLER              REDEFINES IDCATGRP-WS.                           
006400     03  KEY-IDCATGRP            PIC 9(2).                                
006500 01  IDCATAVS-WS                 PIC X(4)    VALUE SPACE.                 
006600 01  FILLER              REDEFINES IDCATAVS-WS.                           
006700     03  KEY-IDCATAVS            PIC 9(4).                                
006800                                                                          
006810 01  TEST-IDCATRAD               PIC 9(4)    VALUE ZERO.                  
006900     EJECT                                                                
007000******************************************************************        
007100*                   NYCKLAR TILL DLI                                      
007200******************************************************************        
007300*                                                                         
007400 01    FILLER                    PIC X(16)                                
007500                                 VALUE 'NYCKLAR TILL DLI'.                
007600 01  NYCKLAR-TILL-DLI.                                                    
007700     SKIP2                                                                
007800   03  W-WDN501KY-X.                                                      
007900       05  W-IDCATNR             PIC 9(5)   VALUE ZERO.                   
008000       05  W-IDCATGRP            PIC 9(2)   VALUE ZERO.                   
008100       05  W-IDCATAVS            PIC 9(4)   VALUE ZERO.                   
008200                                                                          
008300   03  W-WDN512KY-G-X.                                                    
008400       05  W-IDCATRAD-G          PIC 9(4)   VALUE ZERO.                   
008500       05  W-KDCATPUB-G          PIC X(6)   VALUE LOW-VALUE.              
008600                                                                          
008700   03  W-WDN512KY-N-X.                                                    
008800       05  W-IDCATRAD-N          PIC 9(4)   VALUE ZERO.                   
008900       05  W-KDCATPUB-N          PIC X(6)   VALUE LOW-VALUE.              
009000                                                                          
009100   03  W-WDN512KY-19-LO-X.                                                
009200       05  W-IDCATRAD-19-LO      PIC 9(4)   VALUE 19.                     
009300       05  W-KDCATPUB-19-LO      PIC X(6)   VALUE LOW-VALUE.              
009400                                                                          
009500   03  W-WDN512KY-19-HI-X.                                                
009600       05  W-IDCATRAD-19-HI      PIC 9(4)   VALUE 19.                     
009700       05  W-KDCATPUB-19-HI      PIC X(6)   VALUE HIGH-VALUE.             
009800                                                                          
009900   03  W-WDN512KY-8000-X.                                                 
010000       05  W-IDCATRAD-8000       PIC 9(4)   VALUE 8000.                   
010100       05  W-KDCATPUB-8000       PIC X(6)   VALUE LOW-VALUE.              
010200                                                                          
010210   03  W-WDN5G1KY-X.                                                      
010220      05 W-WDN5G1KY-HAEN.                                                 
010230*        --- Hänvisat avsnitts RAD-ADRESS                                 
010240        07 W-WDN5GSEQ-01-X.                                               
010250          09 W-IDCATNR-GSEQ     PIC 9(5)   VALUE ZERO.                    
010260          09 W-IDCATGRP-GSEQ    PIC 9(2)   VALUE ZERO.                    
010270          09 W-IDCATAVS-GSEQ    PIC 9(4)   VALUE ZERO.                    
010280        07 W-WDN5GSEQ-12-X.                                               
010290          09 W-IDCATRAD-GSEQ    PIC 9(4)   VALUE ZERO.                    
010291          09 W-KDCATPUB-GSEQ    PIC X(6)   VALUE SPACE.                   
010292      05 W-IDWDN512-REF-X.                                                
010293*        --- Hänvisande avsnittets RAD-ADRESS                             
010294*        --- FIELD NAME 'IDCATRKY' i det fysiska DBD:t WDN5G              
010295        07 W-WDN501KY-REF.                                                
010296           09 W-IDCATNR-GSEQ-REF PIC 9(5)   VALUE ZERO.                   
010297           09 W-IDCATGRP-GSEQ-REF PIC 9(2)  VALUE ZERO.                   
010298           09 W-IDCATAVS-GSEQ-REF PIC 9(4)  VALUE ZERO.                   
010299        07 W-WDN512KY-REF.                                                
010300           09 W-IDCATRAD-GSEQ-REF PIC 9(4)  VALUE ZERO.                   
010301           09 W-KDCATPUB-GSEQ-REF PIC X(6)  VALUE SPACE.                  
010302                                                                          
010303   03  W-IDCATRKY-LO             PIC X(21) VALUE LOW-VALUE.               
010304   03  W-IDCATRKY-HI             PIC X(21) VALUE HIGH-VALUE.              
010310     EJECT                                                                
010400******************************************************************        
010500*                       TABELL                                            
010600******************************************************************        
010700*                                                                         
010800 01  FILLER                      PIC X(16)                                
010900                                 VALUE 'TABELL'.                          
011000                                                                          
011100 01  TABELL-AREA.                                                         
011500     03  AVS-ANTAL-RADER         PIC 9(4)  VALUE ZERO.                    
011510*        -- Totalt antal rader kvar att omnumrera.                        
011520                                                                          
011600     03  NYTT-RADNR              PIC 9(4)  VALUE 7980.                    
011601*        -- Nästa nya radnummer.                                          
011602                                                                          
011612     03  TAB-START-TRY           PIC S9(4)  VALUE ZERO.                   
011613*        -- Det antal rader man 'VILL' läsa förbi innan start.            
011614                                                                          
011615     03  TAB-START-RAD           PIC 9(4)  VALUE ZERO.                    
011616*        -- Den sekvensrad som omnumreringen 'SKALL' starta från,         
011617*           beroende på lika IDCATRAD-GAMMAL i gränsområdet.              
011618                                                                          
011620     03  TAB-MAX               PIC S9(4)  COMP  VALUE +80.                
011621*        -- Teoretiskt tabell-max.                                        
011622                                                                          
011623     03  FILLER                PIC X(16) VALUE 'TAB-VERKLIGT-MAX'.        
011630     03  TAB-VERKLIGT-MAX      PIC S9(4)  COMP  VALUE +1.                 
011640*        -- Verkligt tabell-max,                                          
011641*           beroende på lika IDCATRAD-GAMMAL i gränsområdet.              
011650     03  IX                    PIC S9(9)  COMP SYNC VALUE +0.             
011700*                                                                         
011800     03  TABELL  OCCURS  1 TO 80 DEPENDING ON TAB-VERKLIGT-MAX.           
011900       05  IDCATRAD-NY           PIC 9(4).                                
012000       05  KDCATPUB-FOM-NY       PIC X(6).                                
012100*                                                                         
012200       05  IDCATRAD-GAMMAL       PIC 9(4).                                
012300       05  KDCATPUB-FOM-GAMMAL   PIC X(6).                                
012400*                                                                         
012500     EJECT                                                                
012600******************************************************************        
012700*                      MEDDELANDE                                         
012800******************************************************************        
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
013100 01  MEDDELANDEN.                                                         
013200                                                                          
013300     03 FILLER-1.                                                         
013400          05 FILLER              PIC X(22)                                
013500              VALUE '    NYCKEL EJ NUMERISK'.                             
013600          05 FILLER              PIC X(22)                                
013700              VALUE '    KEY NOT NUMERIC   '.                             
013800     03 FILLER REDEFINES FILLER-1.                                        
013900          05 FEL-1   OCCURS 2    PIC X(22).                               
014000                                                                          
014100     03 FILLER-2.                                                         
014200          05 FILLER              PIC X(24)                                
014300              VALUE '    AVSNITT SAKNAS      '.                           
014400          05 FILLER              PIC X(24)                                
014500              VALUE '    TEXT-BLOCK NOT FOUND'.                           
014600     03 FILLER REDEFINES FILLER-2.                                        
014700          05 FEL-2   OCCURS 2    PIC X(24).                               
014800                                                                          
014900     03 FILLER-3.                                                         
015000          05 FILLER              PIC X(19)                                
015100              VALUE '    NYCKEL FELAKTIG'.                                
015200          05 FILLER              PIC X(19)                                
015300              VALUE '    WRONG KEY      '.                                
015400     03 FILLER REDEFINES FILLER-3.                                        
015500          05 FEL-3   OCCURS 2    PIC X(19).                               
015600                                                                          
015610     03 FILLER-4.                                                         
015620          05 FILLER              PIC X(24)                                
015630              VALUE '    RADER SAKNAS      '.                             
015640          05 FILLER              PIC X(24)                                
015650              VALUE '    TEXT-LINES NOT FOUND'.                           
015660     03 FILLER REDEFINES FILLER-4.                                        
015670          05 FEL-4   OCCURS 2    PIC X(24).                               
015680                                                                          
015700     03 FILLER-5.                                                         
015800          05 FILLER              PIC X(21)                                
015900              VALUE 'OMNUMRERING GJORD    '.                              
016000          05 FILLER              PIC X(21)                                
016100              VALUE 'RENUMBER IS PERFORMED'.                              
016200     03 FILLER REDEFINES FILLER-5.                                        
016300          05 MED-1   OCCURS 2    PIC X(21).                               
016400                                                                          
016500     03 FILLER-6.                                                         
016600          05 FILLER              PIC X(33)                                
016700              VALUE 'TRYCK PF11 FÖR OMNUMRERING       '.                  
016800          05 FILLER              PIC X(33)                                
016900              VALUE 'PRESS PF11 TO RENUMBER TEXT-BLOCK'.                  
017000     03 FILLER REDEFINES FILLER-6.                                        
017100          05 MED-2   OCCURS 2    PIC X(33).                               
017200     EJECT                                                                
017300******************************************************************        
017400*            AREOR FÖR MFS OCH SKÄRMHANTERING                             
017500******************************************************************        
017600*                                                                         
017700* - - - - - - - - - - - - - - - - - - - - MOD-MID-AREA                    
017800 01  FILLER                      PIC X(16)  VALUE 'MOD-MID-AREA'.         
017900 01  W-PROG-TO-PROG-SW.                                                   
018000     03 M-SW-LL                 PIC S9(4)  VALUE +56  COMP SYNC.          
018100     03 M-SW-Z1-Z2              PIC X(2)   VALUE LOW-VALUE.               
018200     03 M-SW-KDTRANS            PIC X(8)   VALUE 'W1T551U '.              
018300     03 M-SW-IDTRANS            PIC X(4)   VALUE '1551'.                  
018400     03 M-SW-KDMFSFOR           PIC X(1)   VALUE '1'.                     
018500*    03  -COPY W1I55101     -PRE MOD-.                                    
018600     EJECT                                                                
018700* - - - - - - - - - - - - - - - - - - - - MID-AREA                        
018800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018900*01  -COPY W1I55101.                                                      
019000     EJECT                                                                
019100* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
019200*01    -COPY WMSGAREA                                                     
019300     EJECT                                                                
019400* - - - - - - - - - - - - - - - - - - - - MOD-AREA                        
019500*  03    MOD -COPY W1O55101  -RED MSG-AREA.                               
019600     EJECT                                                                
019700* - - - - - - - - - - - - - - - - - - - - MFS-AREA                        
019800*01    -COPY WMFSAREA                                                     
019900     EJECT                                                                
020000******************************************************************        
020100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020200******************************************************************        
020300*                                                                         
020400 01    IMS-WS.                                                            
020500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
020600     SKIP3                                                                
020700*                        **** STATUS-KOD FRÅN IMS                         
020800   03    STATUS-WS               PIC XX.                                  
020900     88    SEGMENT-FINNS                     VALUE '  '.                  
021000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
021100     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
021200     SKIP3                                                                
021300   03    GODK-STATUSKODER.                                                
021400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
021500     SKIP3                                                                
021600 01    SSA1                      PIC X(128).                              
021700 01    SSA2                      PIC X(128).                              
021800 01    SSA3                      PIC X(128).                              
021900     EJECT                                                                
022000*- - - - - - - - - - - - - - IMS FUNKTIONSKODER                           
022100*01    -COPY W0003                                                        
022200     EJECT                                                                
022300*- - - - - - - - - - - - - - DLI INPUT-OUTPUT AREA                        
022301 01    DLI-IO-AREA.                                                       
022310     03  FILLER                  PIC X(16)  VALUE 'AVSG-IO-AREA'.         
022320     03  AVSG-IO-AREA            PIC X(43)  VALUE SPACE.                  
022330*    03 WLKATS01 -COPY WDN5G1  -RED AVSG-IO-AREA.                         
022340                                                                          
022350     03  AVSG-REF-AREA.                                                   
022351*      --- detta är det hänvisande avsnittets adress                      
022352       07 AVSG-REF-WDN501KY.                                              
022360         09 AVSG-REF-IDCATNR       PIC 9(5).                              
022370         09 AVSG-REF-IDCATGRP      PIC 9(2).                              
022380         09 AVSG-REF-IDCATAVS      PIC 9(4).                              
022381       07 AVSG-REF-WDN512KY.                                              
022390         09 AVSG-REF-IDCATRAD      PIC 9(4).                              
022391         09 AVSG-REF-KDCATPUB-FOM  PIC X(6).                              
022392     EJECT                                                                
022393     03  FILLER                   PIC X(16)  VALUE 'IO-AREA1'.            
022500     03    IO-AREA1               PIC X(100)  VALUE SPACE.                
022600     SKIP3                                                                
022700*    03  WLKATH12 -COPY WDN512        -RED IO-AREA1.                      
022800     EJECT                                                                
022900*    03  WLKATH21 -COPY WDN521        -RED IO-AREA1.                      
023000     EJECT                                                                
023100*    03  WLKATH22 -COPY WDN522        -RED IO-AREA1.                      
023200     EJECT                                                                
023300*    03  WLKATH23 -COPY WDN523        -RED IO-AREA1.                      
023400     EJECT                                                                
023500*    03  WLKATH24 -COPY WDN524        -RED IO-AREA1.                      
023600     EJECT                                                                
023700*    03  WLKATH25 -COPY WDN525        -RED IO-AREA1.                      
023800     EJECT                                                                
023900*    03  WLKATH26 -COPY WDN526        -RED IO-AREA1.                      
024000     EJECT                                                                
024100*    03  WLKATH27 -COPY WDN527        -RED IO-AREA1.                      
024200     EJECT                                                                
024500     03    IO-AREA2                PIC X(100)  VALUE SPACE.               
024600     EJECT                                                                
024610     03  FILLER                   PIC X(16)  VALUE 'IO-AREA3'.            
024620     03    IO-AREA3               PIC X(24)  VALUE SPACE.                 
024630     SKIP3                                                                
024699*    03  WLKATH27 -COPY WDN527 -PRE KATH3-  -RED IO-AREA3.                
024700     EJECT                                                                
024710 LINKAGE SECTION.                                                         
024800*01    -COPY W0009     -PRE MSG-                                          
024900     EJECT                                                                
025000*01    -COPY W0008     -PRE ALT-                                          
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300*01    -COPY W0008     -PRE KATH1-                                        
025400     05  FILLER                  PIC X.                                   
025500     EJECT                                                                
025600*01    -COPY W0008     -PRE KATH2-                                        
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025801*01    -COPY W0008     -PRE KATH3-                                        
025802     05  FILLER                  PIC X.                                   
025803     EJECT                                                                
025810*01    -COPY W0008     -PRE KATS-                                         
025820     05  FILLER                  PIC X.                                   
025830     EJECT                                                                
025900 PROCEDURE DIVISION USING MSG-PCB ALT-PCB KATH1-PCB KATH2-PCB             
025910                                 KATH3-PCB KATS-PCB.                      
026000 MAIN SECTION.                                                            
026100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KATH1-PCB KATH2-PCB            
026110                                 KATH3-PCB KATS-PCB.                      
026200                                                                          
026300     PERFORM IMS-GET-MSG                                                  
026400                                                                          
026500     IF SEGMENT-FINNS                                                     
026600       PERFORM A-INIT-SPARA-INPUT                                         
026700       IF (IDCATNR-WS NOT NUMERIC)                                        
026800       OR (IDCATGRP-WS NOT NUMERIC)                                       
026900       OR (IDCATAVS-WS NOT NUMERIC)                                       
027000         MOVE FEL-1 (SPRAAK-IX) TO MOD-MESSAGE-RAD1                       
027100         PERFORM IMS-INSERT-MSG                                           
027200       ELSE                                                               
027300         MOVE KEY-IDCATNR  TO W-IDCATNR                                   
027400         MOVE KEY-IDCATGRP TO W-IDCATGRP                                  
027500         MOVE KEY-IDCATAVS TO W-IDCATAVS                                  
027600                                                                          
027700         IF MFS-UPDATE                                                    
027800           IF OMNUMRERA-8000-RADER = JA                                   
027900             PERFORM D-OMNUMRERA-8000-RADER                               
028000             IF FL-8000-RADER = JA                                        
028010*              --- Omnum av 8000-raderna ej färdig                        
028100               MOVE JA TO MOD-MID-OMNUMRERA-8000-RADER                    
028200                          MOD-MID-8000-RADER                              
028300               PERFORM IMS-INSERT-ALT-MSG                                 
028310*              ---- Starta om !                                           
028400             ELSE                                                         
028500               MOVE MED-1(SPRAAK-IX) TO MOD-MESSAGE-RAD23                 
028600               PERFORM IMS-INSERT-MSG                                     
028700             END-IF                                                       
028800           ELSE                                                           
028900             PERFORM IMS-GU-AVS                                           
029000             IF SEGMENT-FINNS                                             
029002               IF MID-IXHEL = SPACE                                       
029003*                --- Nystart.                                             
029004                 PERFORM B-RAKNA-ANT-RADER                                
029010               ELSE                                                       
029100                 IF MID-IXHEL NUMERIC                                     
029101                 AND MID-IXHEL > ZERO                                     
029103*                  --- Fortsätter här och först när IX blir noll          
029104*                  --- åtgärdas alla ev. 8000-rader.                      
029111                   MOVE MID-NYTT-NR-IDCATRAD TO NYTT-RADNR                
029112*                  --- Nästa nya radnr för sista raden i MID-IXHEL        
029113                   MOVE MID-IXHEL            TO AVS-ANTAL-RADER           
029114*                  --- Det antal rader som är kvar att omnumrera          
029120                 END-IF                                                   
029211               END-IF                                                     
029600               PERFORM C-OMNUMRERA-RADER                                  
029700                                                                          
029800               IF OMNUMRERA-KLAR = JA                                     
029900                 IF FL-8000-RADER = JA                                    
030000                   MOVE JA TO MOD-MID-OMNUMRERA-8000-RADER                
030100                   PERFORM IMS-INSERT-ALT-MSG                             
030110*                  --- Startar om  !                                      
030200                 ELSE                                                     
030300                   MOVE MED-1(SPRAAK-IX) TO MOD-MESSAGE-RAD23             
030400                   PERFORM IMS-INSERT-MSG                                 
030410*                  --- Klart !                                            
030500                 END-IF                                                   
030600               ELSE                                                       
030710                 MOVE FL-8000-RADER TO MOD-MID-8000-RADER                 
030800                 PERFORM IMS-INSERT-ALT-MSG                               
030810*                --- Startar om !                                         
030900               END-IF                                                     
031000             ELSE                                                         
031100               MOVE FEL-2(SPRAAK-IX) TO MOD-MESSAGE-RAD1                  
031200               PERFORM IMS-INSERT-MSG                                     
031210*              ---- Avsnittet finns inte !                                
031300             END-IF                                                       
031400           END-IF                                                         
031500         ELSE                                                             
031600           MOVE MED-2(SPRAAK-IX) TO MOD-MESSAGE-RAD23                     
031700           PERFORM IMS-INSERT-MSG                                         
031710*          ---- ENTER tryckning !                                         
031800         END-IF                                                           
031900       END-IF                                                             
032000     END-IF                                                               
032100     MOVE ZERO TO RETURN-CODE                                             
032200     GOBACK                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 A-INIT-SPARA-INPUT SECTION.                                              
032600     IF MSG-DUBBLA-TRANSKODER                                             
032700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I55101                 
032800                                             MOD-MID-W1I55101             
032900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
033000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033100       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
033200     ELSE                                                                 
033300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I55101                  
033400                                            MOD-MID-W1I55101              
033500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
033600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
033700       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
033800     END-IF                                                               
033900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
034000                                                                          
034100     IF MID-IDCATNR-IN = ALL '+'                                          
034200       MOVE MID-IDCATNR-UT TO IDCATNR-WS                                  
034300       INSPECT IDCATNR-WS REPLACING LEADING SPACE BY ZERO                 
034400     ELSE                                                                 
034500       MOVE MID-IDCATNR-IN TO IDCATNR-WS                                  
034600     END-IF                                                               
034700     IF MID-IDCATGRP-IN = ALL '+'                                         
034800       MOVE MID-IDCATGRP-UT TO IDCATGRP-WS                                
034900       INSPECT IDCATGRP-WS REPLACING LEADING SPACE BY ZERO                
035000     ELSE                                                                 
035100       MOVE MID-IDCATGRP-IN TO IDCATGRP-WS                                
035200     END-IF                                                               
035300     IF MID-IDCATAVS-IN = ALL '+'                                         
035400       MOVE MID-IDCATAVS-UT TO IDCATAVS-WS                                
035500       INSPECT IDCATAVS-WS REPLACING LEADING SPACE BY ZERO                
035600     ELSE                                                                 
035700       MOVE MID-IDCATAVS-IN TO IDCATAVS-WS                                
035800     END-IF                                                               
035900     MOVE MID-8000-RADER           TO FL-8000-RADER                       
036000     MOVE MID-OMNUMRERA-8000-RADER TO OMNUMRERA-8000-RADER                
036100                                                                          
036200     MOVE LOW-VALUE TO MSG-AREA                                           
036300     MOVE 'W1O55101' TO MFS-IDMOD                                         
036400     MOVE '1551' TO MOD-IDTRANS                                           
036500                                                                          
036600     IF ENGLISH-TEXT                                                      
036700       MOVE +2 TO SPRAAK-IX                                               
036800     ELSE                                                                 
036900       MOVE +1 TO SPRAAK-IX                                               
037000     END-IF                                                               
037100     MOVE IDCATNR-WS TO MOD-IDCATNR-UT                                    
037200     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
037300                                                                          
037400     MOVE IDCATGRP-WS TO MOD-IDCATGRP-UT                                  
037500     INSPECT MOD-IDCATGRP-UT REPLACING LEADING ZERO BY SPACE              
037600                                                                          
037700     MOVE IDCATAVS-WS TO MOD-IDCATAVS-UT                                  
037800     INSPECT MOD-IDCATAVS-UT REPLACING LEADING ZERO BY SPACE              
037900                                                                          
038000     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
038100                             MOD-IDCATGRP-IN                              
038200                             MOD-IDCATAVS-IN                              
038300                             MOD-MESSAGE-RAD1                             
038400                             MOD-MESSAGE-RAD23                            
038500                             MOD-NYTT-NR-IDCATRAD                         
038800     .                                                                    
038900                                                                          
039000     EJECT                                                                
039100 B-RAKNA-ANT-RADER    SECTION.                                            
039200     SKIP2                                                                
039300     MOVE +0  TO AVS-ANTAL-RADER                                          
039310                 NYTT-RADNR                                               
039320                                                                          
039400     PERFORM IMS-GNP-AVS-RAD-GAMMAL                                       
039410*    --- läser ALLA rader > 19 på alla pubkoder                           
039500     PERFORM UNTIL  SEGMENT-SAKNAS                                        
039600       ADD +1  TO AVS-ANTAL-RADER                                         
039601                                                                          
039602       IF RAD-IDCATRAD > TEST-IDCATRAD                                    
039603         IF NYTT-RADNR = ZERO                                             
039610           ADD +20 TO NYTT-RADNR                                          
039611         ELSE                                                             
039612           ADD +10 TO NYTT-RADNR                                          
039613         END-IF                                                           
039614         MOVE RAD-IDCATRAD TO TEST-IDCATRAD                               
039620       END-IF                                                             
039700       PERFORM IMS-GNP-AVS-RAD-GAMMAL                                     
039800     END-PERFORM                                                          
039820                                                                          
039900     PERFORM IMS-GU-AVS                                                   
040000     .                                                                    
040100     EJECT                                                                
040200 C-OMNUMRERA-RADER SECTION.                                               
040300     SKIP2                                                                
040420     COMPUTE TAB-START-TRY = AVS-ANTAL-RADER - TAB-MAX + 1                
040422     END-COMPUTE                                                          
040430     IF TAB-START-TRY < +2                                                
040500       MOVE AVS-ANTAL-RADER TO TAB-VERKLIGT-MAX                           
040510       MOVE       +1        TO TAB-START-RAD                              
040520       MOVE       JA        TO OMNUMRERA-KLAR                             
040530     ELSE                                                                 
040531       PERFORM CE-BESTAEM-TAB-START-RAD                                   
040532*      -- Här bestäms också TAB-VERKLIGT-MAX                              
040533*      -- vilket beror på om det finns lika IDCATRAD i starten            
040540       SUBTRACT 1 FROM TAB-START-RAD GIVING MOD-MID-IXHEL                 
040541*        -- Sparar nästa omgångs antal rader att omnumrera                
040600     END-IF                                                               
041600     PERFORM CA-SKRIV-GAMLA-RADER-I-TAB                                   
041900     PERFORM CB-SKRIV-NYA-RADER-I-TAB                                     
042000                                                                          
042010*    --- Omnumrerar baklänges till TABELL-starten                         
042100     MOVE TAB-VERKLIGT-MAX TO IX                                          
042200     PERFORM UNTIL IX = ZERO                                              
042410       IF IDCATRAD-GAMMAL(IX) = IDCATRAD-NY(IX)                           
042500         SUBTRACT 1 FROM IX                                               
042600       ELSE                                                               
042700         MOVE IDCATRAD-GAMMAL    (IX) TO  W-IDCATRAD-G                    
042800         MOVE KDCATPUB-FOM-GAMMAL(IX) TO  W-KDCATPUB-G                    
043000         MOVE IDCATRAD-NY        (IX) TO  W-IDCATRAD-N                    
043100         MOVE KDCATPUB-FOM-NY    (IX) TO  W-KDCATPUB-N                    
043200                                                                          
043300         PERFORM CC-SKAPA-NY-RAD                                          
043400         PERFORM CD-DELETE-GAMMAL-RAD                                     
043500         SUBTRACT 1 FROM IX                                               
043600       END-IF                                                             
043700     END-PERFORM                                                          
043800     .                                                                    
043900     EJECT                                                                
044000 CA-SKRIV-GAMLA-RADER-I-TAB SECTION.                                      
044100     SKIP2                                                                
044121*    --- Läser framlänges till rätt startrad                              
044130     MOVE +1 TO IX                                                        
044140     PERFORM UNTIL  IX >= TAB-START-RAD                                   
044160       PERFORM IMS-GNP-AVS-RAD-GAMMAL                                     
044170       ADD +1 TO IX                                                       
044180     END-PERFORM                                                          
044190*    --- Fyller tabellen uppifrån                                         
044200     MOVE +1 TO IX                                                        
044400     PERFORM UNTIL   IX > TAB-VERKLIGT-MAX                                
044500       PERFORM IMS-GNP-AVS-RAD-GAMMAL                                     
044600       MOVE RAD-IDCATRAD     TO IDCATRAD-GAMMAL(IX)                       
044700       MOVE RAD-KDCATPUB-FOM TO KDCATPUB-FOM-GAMMAL(IX)                   
044800       ADD +1 TO IX                                                       
044900     END-PERFORM                                                          
045000     .                                                                    
045100     EJECT                                                                
045200 CB-SKRIV-NYA-RADER-I-TAB SECTION.                                        
045300     SKIP2                                                                
045400*    --- Kolla de gamla radnumren efter dubbletter                        
045500*    --- isåfall skall de nya också vara dubbletter.                      
045600*    --- Rader med samma radnummer har nämligen olika KDCATPUB-FOM        
045700                                                                          
045800*    --- Nytt radnummer är det som står på tur nedifrån räknat            
045810*    --- Fyller tabellen nedifrån.                                        
045820     MOVE TAB-VERKLIGT-MAX TO IX                                          
045900     PERFORM UNTIL  IX = ZERO                                             
046100*      --- Flyttar radnummer                                              
046200       IF  IX < TAB-VERKLIGT-MAX                                          
046300       AND IDCATRAD-GAMMAL(IX) = IDCATRAD-GAMMAL(IX + 1)                  
046400         MOVE IDCATRAD-NY(IX + 1) TO IDCATRAD-NY(IX)                      
046500       ELSE                                                               
046600         MOVE NYTT-RADNR   TO IDCATRAD-NY(IX)                             
046700         SUBTRACT  +10   FROM NYTT-RADNR                                  
046800       END-IF                                                             
047000*      --- Flyttar Pubkod                                                 
047100       MOVE KDCATPUB-FOM-GAMMAL(IX) TO KDCATPUB-FOM-NY(IX)                
047200       SUBTRACT  +1 FROM  IX                                              
047300     END-PERFORM                                                          
047301*    --- Skriv ut nästa nya IDCATRAD-NY på MOD-MIDDEN                     
047310     MOVE NYTT-RADNR TO MOD-MID-NYTT-NR-IDCATRAD                          
047400     .                                                                    
047500     EJECT                                                                
047600 CC-SKAPA-NY-RAD SECTION.                                                 
047700     SKIP2                                                                
047800     PERFORM IMS-GU-AVS-RAD-GAMMAL                                        
048000     IF SEGMENT-FINNS                                                     
048100        MOVE IDCATRAD-NY(IX)     TO RAD-IDCATRAD                          
048200        MOVE KDCATPUB-FOM-NY(IX) TO RAD-KDCATPUB-FOM                      
048300                                                                          
048400        IF IDCATRAD-NY(IX) < IDCATRAD-GAMMAL(1)                           
048410*         --- Före den första raden i tabellen                            
048420*         --- Lägger den på 'OVERFLOW' tillsvidare                        
048500          COMPUTE RAD-IDCATRAD = (RAD-IDCATRAD / 10) + 8000               
048600          MOVE    RAD-IDCATRAD   TO W-IDCATRAD-N                          
048700          PERFORM IMS-ISRT-AVS-RAD-NY                                     
049000          MOVE JA TO FL-8000-RADER                                        
049100        ELSE                                                              
049110*         --- Försöker lägga in radens nya nummer                         
049200          PERFORM IMS-ISRT-AVS-RAD-NY                                     
049300          IF SEGMENT-FINNS-REDAN                                          
049310*           --- Lägger den på 'OVERFLOW' tillsvidare                      
049400            COMPUTE RAD-IDCATRAD = (RAD-IDCATRAD / 10) + 8000             
049500            MOVE RAD-IDCATRAD   TO W-IDCATRAD-N                           
049600            PERFORM IMS-ISRT-AVS-RAD-NY                                   
049900            MOVE JA TO FL-8000-RADER                                      
050001          END-IF                                                          
050100        END-IF                                                            
050200     END-IF                                                               
050300                                                                          
050400     PERFORM IMS-GHNP-AVS-ART-GAMMAL                                      
050500     IF SEGMENT-FINNS                                                     
050510       MOVE IO-AREA1 TO IO-AREA2                                          
050520       PERFORM IMS-DLET-AVS-SEG-GAMMAL                                    
050600       PERFORM IMS-ISRT-AVS-ART-NY                                        
050800     END-IF                                                               
050900                                                                          
051000     PERFORM IMS-GNP-AVS-TEXT-GAMMAL                                      
051100     IF SEGMENT-FINNS                                                     
051200       PERFORM IMS-ISRT-AVS-TEXT-NY                                       
051400     END-IF                                                               
051500                                                                          
051600     PERFORM IMS-GNP-AVS-BEN-GAMMAL                                       
051700     IF SEGMENT-FINNS                                                     
051800       PERFORM IMS-ISRT-AVS-BEN-NY                                        
052000     END-IF                                                               
052100                                                                          
052200     PERFORM IMS-GNP-AVS-NOT-GAMMAL                                       
052300     PERFORM UNTIL SEGMENT-SAKNAS                                         
052400       PERFORM IMS-ISRT-AVS-NOT-NY                                        
052500       PERFORM IMS-GNP-AVS-NOT-GAMMAL                                     
052700     END-PERFORM                                                          
052800                                                                          
052900     PERFORM IMS-GHNP-AVS-RUB-GAMMAL                                      
053000     PERFORM UNTIL SEGMENT-SAKNAS                                         
053100       MOVE IO-AREA1 TO IO-AREA2                                          
053200       PERFORM IMS-DLET-AVS-SEG-GAMMAL                                    
053300       PERFORM IMS-ISRT-AVS-RUB-NY                                        
053500       PERFORM IMS-GHNP-AVS-RUB-GAMMAL                                    
053600     END-PERFORM                                                          
053700                                                                          
053800     PERFORM IMS-GHNP-AVS-FOT-GAMMAL                                      
053900     PERFORM UNTIL SEGMENT-SAKNAS                                         
054000       MOVE IO-AREA1 TO IO-AREA2                                          
054100       PERFORM IMS-DLET-AVS-SEG-GAMMAL                                    
054200       PERFORM IMS-ISRT-AVS-FOT-NY                                        
054400       PERFORM IMS-GHNP-AVS-FOT-GAMMAL                                    
054500     END-PERFORM                                                          
054600                                                                          
054700     PERFORM IMS-GHNP-AVS-HAEN-GAMMAL                                     
054800     IF SEGMENT-FINNS                                                     
054900       MOVE IO-AREA1 TO IO-AREA2                                          
055000       PERFORM IMS-DLET-AVS-SEG-GAMMAL                                    
055100       PERFORM IMS-ISRT-AVS-HAEN-NY                                       
055300     END-IF                                                               
055501*    Skapa ny referens genom att från hänvisande avsnitt                  
055502*    ändra hänv. från gammal rad till nya.                                
055503*    KATH3-PCB används för detta ändamål.                                 
055504*                                                                         
055505     PERFORM S01-SKRIV-NYA-REFERENS-RADER                                 
056300     .                                                                    
056400     EJECT                                                                
056500 CD-DELETE-GAMMAL-RAD SECTION.                                            
056600     SKIP2                                                                
056700     PERFORM IMS-GHU-AVS-RAD-GAMMAL                                       
056800*    --- Nu finns inga kopplingssegment kvar under raden                  
056900     IF SEGMENT-FINNS                                                     
057000       PERFORM IMS-DLET-AVS-RAD-GAMMAL                                    
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057401 CE-BESTAEM-TAB-START-RAD       SECTION.                                  
057402     SKIP2                                                                
057403*    TAB-START-TRY är nu 80 före sista tidigare omnumrering               
057404*    Kolla om denna IDCATRAD är lika nästa IDCATRAD                       
057405*      -- Här bestäms också TAB-VERKLIGT-MAX                              
057406     MOVE +1 TO IX                                                        
057407     PERFORM UNTIL IX >= TAB-START-TRY                                    
057408*      --- d.v.s. (TAB-START-TRY) rader fram till läs-start               
057409       PERFORM IMS-GNP-AVS-RAD-GAMMAL                                     
057410       ADD +1 TO IX                                                       
057411     END-PERFORM                                                          
057412*    --- Nästa läsning ger den TÄNKTA första tabellraden                  
057413     PERFORM IMS-GNP-AVS-RAD-GAMMAL                                       
057414*    --- Testa den !                                                      
057415     MOVE RAD-IDCATRAD TO TEST-IDCATRAD                                   
057417     MOVE +1 TO IX                                                        
057418*    --- Läs rad nr 2                                                     
057419     PERFORM IMS-GNP-AVS-RAD-GAMMAL                                       
057420     PERFORM UNTIL RAD-IDCATRAD > TEST-IDCATRAD                           
057421       PERFORM IMS-GNP-AVS-RAD-GAMMAL                                     
057422       ADD +1 TO IX                                                       
057423     END-PERFORM                                                          
057424*    --- Nu hittades nästa UNIKA rad. Börja omnumrera från den            
057425     ADD      IX TO   TAB-START-TRY GIVING TAB-START-RAD                  
057426*    --- Korta längden på tabellen lika mycket !                          
057427     SUBTRACT IX FROM TAB-MAX       GIVING TAB-VERKLIGT-MAX               
057428*    --- Positionera rätt igen !                                          
057429     PERFORM IMS-GU-AVS                                                   
057430     .                                                                    
057440     EJECT                                                                
057500 D-OMNUMRERA-8000-RADER SECTION.                                          
057600     SKIP2                                                                
057721                                                                          
057730     MOVE  +1 TO IX                                                       
057800     PERFORM IMS-GU-8000-RADER                                            
057900*    --- W-WDN512KY-8000-X     --- IO-AREA-1,  KATH1-PCB                  
058000     PERFORM UNTIL  SEGMENT-SAKNAS OR IX > +49                            
058100                                                                          
058200       MOVE RAD-IDCATRAD     TO W-IDCATRAD-G                              
058210       MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-G                              
058300*      --- Förbereder borttag av läst 8000-nummer                         
058500       COMPUTE RAD-IDCATRAD = (RAD-IDCATRAD - 8000) * 10                  
058600*      --- Återställer nya radnumret till det rätta värdet                
058700                                                                          
058800       MOVE RAD-IDCATRAD     TO W-IDCATRAD-N                              
058810       MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-N                              
058900       PERFORM IMS-ISRT-AVS-RAD-NY                                        
059100                                                                          
059200       PERFORM IMS-GHNP-AVS-ART-GAMMAL                                    
059300       IF SEGMENT-FINNS                                                   
059310         MOVE IO-AREA1 TO IO-AREA2                                        
059320         PERFORM IMS-DLET-AVS-SEG-GAMMAL                                  
059400         PERFORM IMS-ISRT-AVS-ART-NY                                      
059600       END-IF                                                             
059700                                                                          
059800       PERFORM IMS-GNP-AVS-TEXT-GAMMAL                                    
059900       IF SEGMENT-FINNS                                                   
060000         PERFORM IMS-ISRT-AVS-TEXT-NY                                     
060200       END-IF                                                             
060300                                                                          
060400       PERFORM IMS-GNP-AVS-BEN-GAMMAL                                     
060500       IF SEGMENT-FINNS                                                   
060600         PERFORM IMS-ISRT-AVS-BEN-NY                                      
060800       END-IF                                                             
060900                                                                          
061000       PERFORM IMS-GNP-AVS-NOT-GAMMAL                                     
061100       PERFORM UNTIL SEGMENT-SAKNAS                                       
061200         PERFORM IMS-ISRT-AVS-NOT-NY                                      
061400         PERFORM IMS-GNP-AVS-NOT-GAMMAL                                   
061500       END-PERFORM                                                        
061600                                                                          
061700       PERFORM IMS-GHNP-AVS-RUB-GAMMAL                                    
061800       PERFORM UNTIL SEGMENT-SAKNAS                                       
061900         MOVE IO-AREA1 TO IO-AREA2                                        
062000         PERFORM IMS-DLET-AVS-SEG-GAMMAL                                  
062100         PERFORM IMS-ISRT-AVS-RUB-NY                                      
062300         PERFORM IMS-GHNP-AVS-RUB-GAMMAL                                  
062400       END-PERFORM                                                        
062500                                                                          
062600       PERFORM IMS-GHNP-AVS-FOT-GAMMAL                                    
062700       PERFORM UNTIL SEGMENT-SAKNAS                                       
062800         MOVE IO-AREA1 TO IO-AREA2                                        
062900         PERFORM IMS-DLET-AVS-SEG-GAMMAL                                  
063000         PERFORM IMS-ISRT-AVS-FOT-NY                                      
063200         PERFORM IMS-GHNP-AVS-FOT-GAMMAL                                  
063300       END-PERFORM                                                        
063400                                                                          
063500       PERFORM IMS-GHNP-AVS-HAEN-GAMMAL                                   
063600       IF SEGMENT-FINNS                                                   
063700         MOVE IO-AREA1 TO IO-AREA2                                        
063800         PERFORM IMS-DLET-AVS-SEG-GAMMAL                                  
063900         PERFORM IMS-ISRT-AVS-HAEN-NY                                     
064100       END-IF                                                             
064101                                                                          
064110       PERFORM S01-SKRIV-NYA-REFERENS-RADER                               
064120                                                                          
065000       PERFORM DA-DELETE-GAMMAL-RAD                                       
065100                                                                          
065200       PERFORM IMS-GU-8000-RADER                                          
065300*      --- Läser nästa "nya" sparade 8000-rad                             
065400     END-PERFORM                                                          
065500                                                                          
065600     IF SEGMENT-SAKNAS                                                    
065700       MOVE NEJ TO FL-8000-RADER                                          
065800     END-IF                                                               
065900     .                                                                    
066000     EJECT                                                                
066100 DA-DELETE-GAMMAL-RAD SECTION.                                            
066200     SKIP2                                                                
066300     PERFORM IMS-GHU-AVS-RAD-GAMMAL                                       
066400     IF SEGMENT-FINNS                                                     
066500       PERFORM IMS-DLET-AVS-RAD-GAMMAL                                    
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
066910 S01-SKRIV-NYA-REFERENS-RADER SECTION.                                    
066920     SKIP2                                                                
066930     MOVE W-WDN501KY-X    TO  W-WDN5GSEQ-01-X                             
066940     MOVE W-WDN512KY-G-X  TO  W-WDN5GSEQ-12-X                             
066941*    --- Spara hänvisningsadressen i WS                                   
066942     MOVE W-WDN5G1KY-HAEN TO   IMS-HAEN-ID                                
066943                                                                          
066950*    --- Läs WDN5G1 med hänvisat avsnitt (detta) för att få REF.          
066960     PERFORM IMS-GU-KATS-REF-GAMMAL                                       
066970     PERFORM UNTIL SEGMENT-SAKNAS                                         
066980*      --- flytta ref. nycklar för läsning med KATH3-PCB                  
066990       MOVE AVSG-IDWDN512 TO AVSG-REF-AREA                                
066991                             IMS-REF-ID                                   
066992*      --- Läs hänvisnade avsnitts HAEN-seg                               
066993       PERFORM IMS-GHU-KATH3-REF                                          
066994*      --- Byt ut radnumret i IO-arean till det nya                       
066995       MOVE W-IDCATRAD-N TO KATH3-HAEN-IDCATRAD                           
066996*      --- Tag först bort den gamla hänvisningen hit                      
066997       PERFORM IMS-DLET-AVS-SEG-GAMMAL-KATH3                              
066998*      --- Lägg upp en ny. Ny SEQ-index-post skapas på WDN5G1             
066999       PERFORM IMS-ISRT-AVS-HAEN-KATH3                                    
067001*      --- Läs nästa WDN5G1-post                                          
067002       PERFORM IMS-GN-KATS-REF-GAMMAL                                     
067003     END-PERFORM                                                          
067004     .                                                                    
067005     EJECT                                                                
067010* IMS SEKTIONER                                                           
067100     SKIP3                                                                
067200 IMS-GET-MSG SECTION.                                                     
067300     MOVE '  QC' TO GODK-STATUSKODER                                      
067400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
067500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     .                                                                    
067800     SKIP3                                                                
067900 IMS-INSERT-MSG SECTION.                                                  
068000     SKIP2                                                                
068100     ADD LENGTH OF MOD-W1O55101 +4 GIVING MSG-KVLL                        
068200     IF ENGLISH-TEXT                                                      
068300       MOVE 'N' TO MFS-KDHUVOMR                                           
068400     END-IF                                                               
068500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
068600     MOVE SPACE TO GODK-STATUSKODER                                       
068700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA    MFS-IDMOD             
068800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068900     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069100     SKIP3                                                                
069200 IMS-INSERT-ALT-MSG SECTION.                                              
069300     SKIP2                                                                
069500     MOVE SPACE TO GODK-STATUSKODER                                       
069600     IF ENGLISH-TEXT                                                      
069700       MOVE '2' TO M-SW-KDMFSFOR                                          
069800     END-IF                                                               
069900     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
070000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
070100     PERFORM IMS-STATUSKONTROLL                                           
070200     .                                                                    
070300     EJECT                                                                
070400 IMS-GU-AVS SECTION.                                                      
070410     MOVE 'IMS-GU-AVS '              TO IMS-CALL-ID                       
070500     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
070600            DELIMITED BY SIZE INTO SSA1                                   
070700     MOVE '  GE' TO GODK-STATUSKODER                                      
070800     CALL CBLTDLI USING GU KATH1-PCB IO-AREA1 SSA1                        
070900     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200     EJECT                                                                
071300 IMS-GNP-AVS-RAD-GAMMAL SECTION.                                          
071310     MOVE 'IMS-GNP-AVS-RAD-GAMMAL'   TO IMS-CALL-ID                       
071400     STRING 'WLKATH12(WDN512KY >' W-WDN512KY-19-LO-X ')'                  
071500                                                                          
071600            DELIMITED BY SIZE INTO SSA1                                   
071700     MOVE '  GE' TO GODK-STATUSKODER                                      
071800     CALL CBLTDLI USING GNP KATH1-PCB IO-AREA1 SSA1                       
071900     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
072000     PERFORM IMS-STATUSKONTROLL                                           
072100     .                                                                    
072200     SKIP3                                                                
072300 IMS-GHU-AVS-RAD-GAMMAL SECTION.                                          
072310     MOVE 'IMS-GHU-AVS-RAD-GAMMAL'   TO IMS-CALL-ID                       
072400     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
072500            DELIMITED BY SIZE INTO SSA1                                   
072600     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
072700            DELIMITED BY SIZE INTO SSA2                                   
072800     MOVE '  GE' TO GODK-STATUSKODER                                      
072900     CALL CBLTDLI USING GHU KATH1-PCB IO-AREA1 SSA1 SSA2                  
073000     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300     SKIP3                                                                
073400 IMS-GU-AVS-RAD-GAMMAL SECTION.                                           
073410     MOVE 'IMS-GU-AVS-RAD-GAMMAL '   TO IMS-CALL-ID                       
073500     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
073600            DELIMITED BY SIZE INTO SSA1                                   
073700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
073800            DELIMITED BY SIZE INTO SSA2                                   
073900     MOVE '  GE' TO GODK-STATUSKODER                                      
074000     CALL CBLTDLI USING GU KATH1-PCB IO-AREA1 SSA1 SSA2                   
074100     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
074200     PERFORM IMS-STATUSKONTROLL                                           
074300     .                                                                    
074400     EJECT                                                                
074500 IMS-GU-8000-RADER SECTION.                                               
074510     MOVE 'IMS-GU-8000-RADER     '   TO IMS-CALL-ID                       
074600     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
074700            DELIMITED BY SIZE INTO SSA1                                   
074800     STRING 'WLKATH12(WDN512KY>=' W-WDN512KY-8000-X ')'                   
074900            DELIMITED BY SIZE INTO SSA2                                   
075000     MOVE '  GE' TO GODK-STATUSKODER                                      
075100     CALL CBLTDLI USING GU KATH1-PCB IO-AREA1 SSA1 SSA2                   
075200     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
075300     PERFORM IMS-STATUSKONTROLL                                           
075400     .                                                                    
075500     EJECT                                                                
075600*                                                                         
075700 IMS-GHNP-AVS-ART-GAMMAL SECTION.                                         
075710     MOVE 'IMS-GHNP-AVS-ART-GAMMAL'  TO IMS-CALL-ID                       
075800     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
075900            DELIMITED BY SIZE INTO SSA1                                   
076000     MOVE 'WLKATH21 ' TO SSA2                                             
076100     MOVE '  GE' TO GODK-STATUSKODER                                      
076200     CALL CBLTDLI USING GHNP KATH1-PCB IO-AREA1 SSA1 SSA2                 
076300     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
076400     PERFORM IMS-STATUSKONTROLL                                           
076500     .                                                                    
076600     EJECT                                                                
076700 IMS-GNP-AVS-TEXT-GAMMAL SECTION.                                         
076710     MOVE 'IMS-GNP-AVS-TEXT-GAMMAL'  TO IMS-CALL-ID                       
076800     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
076900            DELIMITED BY SIZE INTO SSA1                                   
077000     MOVE 'WLKATH22 ' TO SSA2                                             
077100     MOVE '  GE' TO GODK-STATUSKODER                                      
077200     CALL CBLTDLI USING GNP KATH1-PCB IO-AREA1 SSA1 SSA2                  
077300     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
077400     PERFORM IMS-STATUSKONTROLL                                           
077500     .                                                                    
077600     EJECT                                                                
077700 IMS-GNP-AVS-BEN-GAMMAL SECTION.                                          
077710     MOVE 'IMS-GNP-AVS-BEN-GAMMAL '  TO IMS-CALL-ID                       
077800     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
077900            DELIMITED BY SIZE INTO SSA1                                   
078000     MOVE 'WLKATH23 ' TO SSA2                                             
078100     MOVE '  GE' TO GODK-STATUSKODER                                      
078200     CALL CBLTDLI USING GNP KATH1-PCB IO-AREA1 SSA1 SSA2                  
078300     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
078400     PERFORM IMS-STATUSKONTROLL                                           
078500     .                                                                    
078600     EJECT                                                                
078700 IMS-GNP-AVS-NOT-GAMMAL SECTION.                                          
078710     MOVE 'IMS-GNP-AVS-NOT-GAMMAL '  TO IMS-CALL-ID                       
078800     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
078900            DELIMITED BY SIZE INTO SSA1                                   
079000     MOVE 'WLKATH24 ' TO SSA2                                             
079100     MOVE '  GE' TO GODK-STATUSKODER                                      
079200     CALL CBLTDLI USING GNP KATH1-PCB IO-AREA1 SSA1 SSA2                  
079300     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
079400     PERFORM IMS-STATUSKONTROLL                                           
079500     .                                                                    
079600     EJECT                                                                
079700*                                                                         
079800 IMS-GHNP-AVS-RUB-GAMMAL  SECTION.                                        
079810     MOVE 'IMS-GHNP-AVS-RUB-GAMMAL'  TO IMS-CALL-ID                       
079900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
080000            DELIMITED BY SIZE INTO SSA1                                   
080100     MOVE 'WLKATH25 ' TO SSA2                                             
080200     MOVE '  GE' TO GODK-STATUSKODER                                      
080300     CALL CBLTDLI USING GHNP KATH1-PCB IO-AREA1 SSA1 SSA2                 
080400     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
080500     PERFORM IMS-STATUSKONTROLL                                           
080600     .                                                                    
080700     EJECT                                                                
080800 IMS-GHNP-AVS-FOT-GAMMAL  SECTION.                                        
080810     MOVE 'IMS-GHNP-AVS-FOT-GAMMAL'  TO IMS-CALL-ID                       
080900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
081000            DELIMITED BY SIZE INTO SSA1                                   
081100     MOVE 'WLKATH26 ' TO SSA2                                             
081200     MOVE '  GE' TO GODK-STATUSKODER                                      
081300     CALL CBLTDLI USING GHNP KATH1-PCB IO-AREA1 SSA1 SSA2                 
081400     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
081500     PERFORM IMS-STATUSKONTROLL                                           
081600     .                                                                    
081700     EJECT                                                                
081800 IMS-GHNP-AVS-HAEN-GAMMAL SECTION.                                        
081810     MOVE 'IMS-GHNP-AVS-HAEN-GAMMAL' TO IMS-CALL-ID                       
081900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-G-X ')'                      
082000            DELIMITED BY SIZE INTO SSA1                                   
082100     MOVE 'WLKATH27 ' TO SSA2                                             
082200     MOVE '  GE' TO GODK-STATUSKODER                                      
082300     CALL CBLTDLI USING GHNP KATH1-PCB IO-AREA1 SSA1 SSA2                 
082400     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
082500     PERFORM IMS-STATUSKONTROLL                                           
082600     .                                                                    
082700     EJECT                                                                
083710 IMS-GU-KATS-REF-GAMMAL SECTION.                                          
083711     MOVE 'IMS-GU-KATS-REF-GAMMAL  ' TO IMS-CALL-ID                       
083720     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5G1KY-HAEN                         
083730                                  W-IDCATRKY-LO                           
083740                 OCH 'WDN5G1KY<=' W-WDN5G1KY-HAEN                         
083741                                  W-IDCATRKY-HI ')'                       
083742            DELIMITED BY SIZE INTO SSA1                                   
083750     MOVE '  GE' TO GODK-STATUSKODER                                      
083760     CALL CBLTDLI USING GU KATS-PCB AVSG-IO-AREA SSA1                     
083770     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
083780     PERFORM IMS-STATUSKONTROLL                                           
083790     .                                                                    
083791     EJECT                                                                
083802 IMS-GN-KATS-REF-GAMMAL SECTION.                                          
083803     MOVE 'IMS-GN-KATS-REF-GAMMAL  ' TO IMS-CALL-ID                       
083804     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5G1KY-HAEN                         
083805                                  W-IDCATRKY-LO                           
083806                 OCH 'WDN5G1KY<=' W-WDN5G1KY-HAEN                         
083807                                  W-IDCATRKY-HI ')'                       
083808            DELIMITED BY SIZE INTO SSA1                                   
083809     MOVE '  GE' TO GODK-STATUSKODER                                      
083810     CALL CBLTDLI USING GN KATS-PCB AVSG-IO-AREA SSA1                     
083811     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
083812     PERFORM IMS-STATUSKONTROLL                                           
083813     .                                                                    
083814     EJECT                                                                
083820*                                                                         
083900 IMS-DLET-AVS-RAD-GAMMAL SECTION.                                         
083910     MOVE 'IMS-DLET-AVS-RAD-GAMMAL ' TO IMS-CALL-ID                       
084000     MOVE '  ' TO GODK-STATUSKODER                                        
084100     CALL CBLTDLI USING DLET KATH1-PCB IO-AREA1                           
084200     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
084300     PERFORM IMS-STATUSKONTROLL                                           
084400     .                                                                    
084500     EJECT                                                                
084600 IMS-DLET-AVS-SEG-GAMMAL SECTION.                                         
084610     MOVE 'IMS-DLET-AVS-SEG-GAMMAL ' TO IMS-CALL-ID                       
084700     MOVE '  ' TO GODK-STATUSKODER                                        
084800     CALL CBLTDLI USING DLET KATH1-PCB IO-AREA1                           
084900     MOVE KATH1-STATUS-CODE TO STATUS-WS                                  
085000     PERFORM IMS-STATUSKONTROLL                                           
085100     .                                                                    
085200     EJECT                                                                
085300*                                                                         
085400 IMS-ISRT-AVS-RAD-NY SECTION.                                             
085410     MOVE 'IMS-ISRT-AVS-RAD-NY     ' TO IMS-CALL-ID                       
085500     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
085600            DELIMITED BY SIZE INTO SSA1                                   
085700     MOVE 'WLKATH12 ' TO SSA2                                             
085800     MOVE '  II' TO GODK-STATUSKODER                                      
085900     CALL CBLTDLI USING ISRT KATH2-PCB IO-AREA1 SSA1 SSA2                 
086000     MOVE KATH2-STATUS-CODE TO STATUS-WS                                  
086100     PERFORM IMS-STATUSKONTROLL                                           
086200     .                                                                    
086300     EJECT                                                                
086400 IMS-ISRT-AVS-ART-NY   SECTION.                                           
086410     MOVE 'IMS-ISRT-AVS-ART-NY     ' TO IMS-CALL-ID                       
086500     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
086600            DELIMITED BY SIZE INTO SSA1                                   
086700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-N-X ')'                      
086800            DELIMITED BY SIZE INTO SSA2                                   
086900     MOVE 'WLKATH21 ' TO SSA3                                             
087000     MOVE '  ' TO GODK-STATUSKODER                                        
087100     CALL CBLTDLI USING ISRT KATH2-PCB IO-AREA1 SSA1 SSA2 SSA3            
087200     MOVE KATH2-STATUS-CODE TO STATUS-WS                                  
087300     PERFORM IMS-STATUSKONTROLL                                           
087400     .                                                                    
087500     EJECT                                                                
087600 IMS-ISRT-AVS-TEXT-NY  SECTION.                                           
087610     MOVE 'IMS-ISRT-AVS-TEXT-NY     ' TO IMS-CALL-ID                      
087700     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
087800            DELIMITED BY SIZE INTO SSA1                                   
087900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-N-X ')'                      
088000            DELIMITED BY SIZE INTO SSA2                                   
088100     MOVE 'WLKATH22 ' TO SSA3                                             
088200     MOVE '  ' TO GODK-STATUSKODER                                        
088300     CALL CBLTDLI USING ISRT KATH2-PCB IO-AREA1 SSA1 SSA2 SSA3            
088400     MOVE KATH2-STATUS-CODE TO STATUS-WS                                  
088500     PERFORM IMS-STATUSKONTROLL                                           
088600     .                                                                    
088700     EJECT                                                                
088800 IMS-ISRT-AVS-BEN-NY   SECTION.                                           
088810     MOVE 'IMS-ISRT-AVS-BEN-NY     ' TO IMS-CALL-ID                       
088900     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
089000            DELIMITED BY SIZE INTO SSA1                                   
089100     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-N-X ')'                      
089200            DELIMITED BY SIZE INTO SSA2                                   
089300     MOVE 'WLKATH23 ' TO SSA3                                             
089400     MOVE '  ' TO GODK-STATUSKODER                                        
089500     CALL CBLTDLI USING ISRT KATH2-PCB IO-AREA1 SSA1 SSA2 SSA3            
089600     MOVE KATH2-STATUS-CODE TO STATUS-WS                                  
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     .                                                                    
089900     EJECT                                                                
090000 IMS-ISRT-AVS-NOT-NY   SECTION.                                           
090010     MOVE 'IMS-ISRT-AVS-NOT-NY     ' TO IMS-CALL-ID                       
090100     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
090200            DELIMITED BY SIZE INTO SSA1                                   
090300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-N-X ')'                      
090400            DELIMITED BY SIZE INTO SSA2                                   
090500     MOVE 'WLKATH24 ' TO SSA3                                             
090600     MOVE '  ' TO GODK-STATUSKODER                                        
090700     CALL CBLTDLI USING ISRT KATH2-PCB IO-AREA1 SSA1 SSA2 SSA3            
090800     MOVE KATH2-STATUS-CODE TO STATUS-WS                                  
090900     PERFORM IMS-STATUSKONTROLL                                           
091000     .                                                                    
091100     EJECT                                                                
091200 IMS-ISRT-AVS-RUB-NY   SECTION.                                           
091210     MOVE 'IMS-ISRT-AVS-RUB-NY     ' TO IMS-CALL-ID                       
091300     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
091400            DELIMITED BY SIZE INTO SSA1                                   
091500     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-N-X ')'                      
091600            DELIMITED BY SIZE INTO SSA2                                   
091700     MOVE 'WLKATH25 ' TO SSA3                                             
091800     MOVE '  ' TO GODK-STATUSKODER                                        
091900     CALL CBLTDLI USING ISRT KATH2-PCB IO-AREA2 SSA1 SSA2 SSA3            
092000     MOVE KATH2-STATUS-CODE TO STATUS-WS                                  
092100     PERFORM IMS-STATUSKONTROLL                                           
092200     .                                                                    
092300     EJECT                                                                
092400 IMS-ISRT-AVS-FOT-NY   SECTION.                                           
092410     MOVE 'IMS-ISRT-AVS-FOT-NY     ' TO IMS-CALL-ID                       
092500     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
092600            DELIMITED BY SIZE INTO SSA1                                   
092700     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-N-X ')'                      
092800            DELIMITED BY SIZE INTO SSA2                                   
092900     MOVE 'WLKATH26 ' TO SSA3                                             
093000     MOVE '  ' TO GODK-STATUSKODER                                        
093100     CALL CBLTDLI USING ISRT KATH2-PCB IO-AREA2 SSA1 SSA2 SSA3            
093200     MOVE KATH2-STATUS-CODE TO STATUS-WS                                  
093300     PERFORM IMS-STATUSKONTROLL                                           
093400     .                                                                    
093500     EJECT                                                                
093600 IMS-ISRT-AVS-HAEN-NY  SECTION.                                           
093610     MOVE 'IMS-ISRT-AVS-HAEN-NY     ' TO IMS-CALL-ID                      
093700     STRING 'WLKATH01(WDN501KY =' W-WDN501KY-X ')'                        
093800            DELIMITED BY SIZE INTO SSA1                                   
093900     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-N-X ')'                      
094000            DELIMITED BY SIZE INTO SSA2                                   
094100     MOVE 'WLKATH27 ' TO SSA3                                             
094200     MOVE '  ' TO GODK-STATUSKODER                                        
094300     CALL CBLTDLI USING ISRT KATH2-PCB IO-AREA2 SSA1 SSA2 SSA3            
094400     MOVE KATH2-STATUS-CODE TO STATUS-WS                                  
094500     PERFORM IMS-STATUSKONTROLL                                           
094600     .                                                                    
094700     EJECT                                                                
094701 IMS-GHU-KATH3-REF  SECTION.                                              
094702     MOVE 'IMS-GHU-KATH3-REF        ' TO IMS-CALL-ID                      
094703     STRING 'WLKATH01(WDN501KY =' AVSG-REF-WDN501KY ')'                   
094704            DELIMITED BY SIZE INTO SSA1                                   
094705     STRING 'WLKATH12(WDN512KY =' AVSG-REF-WDN512KY ')'                   
094706            DELIMITED BY SIZE INTO SSA2                                   
094707     STRING 'WLKATH27(WDN527KY =' AVSG-IDCATRKY  ')'                      
094708            DELIMITED BY SIZE INTO SSA3                                   
094709     MOVE '  ' TO GODK-STATUSKODER                                        
094710     CALL CBLTDLI USING GHU KATH3-PCB IO-AREA3 SSA1 SSA2 SSA3             
094711     MOVE KATH3-STATUS-CODE TO STATUS-WS                                  
094712     PERFORM IMS-STATUSKONTROLL                                           
094713     .                                                                    
094714     EJECT                                                                
094715 IMS-DLET-AVS-SEG-GAMMAL-KATH3 SECTION.                                   
094716     MOVE 'IMS-DLET-AVS-SEG-GAMMAL-KATH3' TO IMS-CALL-ID                  
094717     MOVE '  ' TO GODK-STATUSKODER                                        
094718     CALL CBLTDLI USING DLET KATH3-PCB IO-AREA3                           
094719     MOVE KATH3-STATUS-CODE TO STATUS-WS                                  
094720     PERFORM IMS-STATUSKONTROLL                                           
094721     .                                                                    
094722     EJECT                                                                
094723 IMS-ISRT-AVS-HAEN-KATH3  SECTION.                                        
094724     MOVE 'IMS-ISRT-AVS-HAEN-KATH3      ' TO IMS-CALL-ID                  
094725     STRING 'WLKATH01(WDN501KY =' AVSG-REF-WDN501KY ')'                   
094730            DELIMITED BY SIZE INTO SSA1                                   
094740     STRING 'WLKATH12(WDN512KY =' AVSG-REF-WDN512KY ')'                   
094750            DELIMITED BY SIZE INTO SSA2                                   
094760     MOVE 'WLKATH27 ' TO SSA3                                             
094770     MOVE '  ' TO GODK-STATUSKODER                                        
094780     CALL CBLTDLI USING ISRT KATH3-PCB IO-AREA3 SSA1 SSA2 SSA3            
094790     MOVE KATH3-STATUS-CODE TO STATUS-WS                                  
094791     PERFORM IMS-STATUSKONTROLL                                           
094792     .                                                                    
094793     EJECT                                                                
096000                                                                          
096100 IMS-STATUSKONTROLL SECTION.                                              
096200     SET STATUS-IX TO 1                                                   
096300     SEARCH GODK-STATUS                                                   
096400       AT END                                                             
096500         CALL FELLOG                                                      
096600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
096700       CONTINUE                                                           
096800     END-SEARCH                                                           
096900     .                                                                    
