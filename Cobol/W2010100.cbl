000100 PROCESS DYNAM                                                            
000110 ID DIVISION.                                                             
000200 PROGRAM-ID.                W2010100.                                     
000300 AUTHOR.                    IDK, GÖTEBORG.                                
000400 DATE-COMPILED.                                                           
000500 DATE-WRITTEN.              APRIL -79.                                    
000600     REMARKS.                                                             
000700*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
000800*                'ARTIKELINFO - ANSK'                                     
000900     INDATA.                                                              
001000         TRANSAKTION: W2T101                                              
001100         MID:         W2I10101                                            
001200     UTDATA.                                                              
001300         MOD:         W2O10101                                            
001400         FELLOG                                                           
001500*                                                                         
001600*   ÄNDRINGAR:                                                            
001700*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
001800*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
001900*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002000*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002100*                                                                         
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700     SKIP3                                                                
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003020 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
003030 77      WS-DB2-SEKTION  PIC X(24)   VALUE SPACE.                         
003100     SKIP1                                                                
003200 77      JA              PIC X       VALUE 'J'.                           
003300 77      NEJ             PIC X       VALUE 'N'.                           
003400 77      STRECK          PIC X       VALUE '-'.                           
003500 77      WS-IDLEVNR-8    PIC X(8)    VALUE SPACE.                         
003600 77      SPIND           PIC S9(9)   VALUE +0   COMP SYNC.                
003700 77      INDX            PIC S9(9)   VALUE +0    COMP SYNC.               
003800     SKIP1                                                                
003900 77      MAX-MOD-LENGD   PIC S9(4)   VALUE +545  COMP SYNC.               
004000 77      MAX-ANT-KAT-TILLH                                                
004100                         PIC S9(9)   VALUE +11   COMP SYNC.               
004200 77      MAX-ANT-AO-NUMMER                                                
004300                         PIC S9(9)   VALUE +5    COMP SYNC.               
004400 77      MAX-ANT-PROENH                                                   
004500                         PIC S9(9)   VALUE +3    COMP SYNC.               
004600 77      WS-FLGEMART     PIC X(1)    VALUE 'N'.                           
004700                                                                          
004800     SKIP3                                                                
004900 01      BLADDRINGS-FALT.                                                 
005000   03    TOT-KAT         PIC 9(3)   VALUE ZERO.                           
005100   03    LASTA-SEG       PIC 9(3)   VALUE ZERO.                           
005200     SKIP3                                                                
005300 01      W-IDARTNR-X.                                                     
005400   03    W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
005500 01      W-KDCLAGER-X.                                                    
005600   03    W-KDCLAGER      PIC S9(1)   VALUE ZERO  COMP-3.                  
005700 01      W-KDSEGKEY-X.                                                    
005800   03    W-KDSEGKEY      PIC X       VALUE SPACE.                         
005900 01      W-IDSKYLT-X.                                                     
006000   03    W-IDSKYLT       PIC X(3)    VALUE SPACE.                         
006100 01      W-KDNOTTYP-X.                                                    
006200   03    W-KDNOTTYP      PIC S9(1)   VALUE 1 COMP-3.                      
006201                                                                          
006202 01      W-WDGXKEY-1143-X.                                                
006203   03    W-IDHTYP-1143   PIC X(4)    VALUE '1143'.                        
006204   03    FILLER          PIC X(26)   VALUE LOW-VALUE.                     
006205                                                                          
006210 01      W-IDFKNGRP-FOM-X.                                                
006220   03    W-IDFKNGRP-FOM  PIC S9(5) COMP-3 VALUE +0.                       
006230                                                                          
006240 01      W-IDFKNGRP-TOM-X.                                                
006250   03    W-IDFKNGRP-TOM  PIC S9(5) COMP-3 VALUE +0.                       
006300     SKIP3                                                                
006400 01      SWITCHAR.                                                        
006500   03    SW-NYCKLAR-OK   PIC X       VALUE 'N'.                           
006600     SKIP3                                                                
006700 01  W.                                                                   
006800     03  IX                  PIC S9(9)               COMP SYNC.           
006900     03  IY                  PIC S9(9)               COMP SYNC.           
007000     03  IZ                  PIC S9(9)               COMP SYNC.           
007100     SKIP1                                                                
007200     03  W-KATALOG-ANT       PIC S9(3)               COMP-3.              
007300     03  W-FORSTA-KATALOG-ANT                                             
007400                             PIC S9(3)               COMP-3.              
007500     SKIP1                                                                
007600*  BORTTAG AV KONTO INFÖR SAP  980901  GS*****************                
007700*    03  W-IDLKTO-N          PIC 9(7).                                    
007800*    03  FILLER              REDEFINES W-IDLKTO-N.                        
007900*        05  FILLER          PIC 9(2).                                    
008000*        05  W-IDLKTO-3-5    PIC 9(3).                                    
008100*        05  W-IDLKTO-6-7    PIC 9(2).                                    
008200*    SKIP1                                                                
008300*    03  W-IDLKTO-FLYTT-N    PIC 9(6).                                    
008400*    03  FILLER              REDEFINES W-IDLKTO-FLYTT-N.                  
008500*        05  W-IDLKTO-3-5-FLYTT    PIC 9(3).                              
008600*        05  FILLER-STRECK         PIC X(1).                              
008700*        05  W-IDLKTO-6-7-FLYTT    PIC 9(2).                              
008800**********************************************************                
008900     SKIP1                                                                
009000     03  W-KDLEVSP-N         PIC 9(2).                                    
009100     03  FILLER              REDEFINES W-KDLEVSP-N.                       
009200         05  W-KDLEVSP-1     PIC 9.                                       
009300         05  W-KDLEVSP-2     PIC 9.                                       
009400     SKIP1                                                                
009500     03  W-KDFARLIG-X.                                                    
009600         05  W-KDFARLIG      PIC 9.                                       
009700     SKIP1                                                                
009800 01  W-NOTTYP-3              PIC S9  VALUE +3  COMP-3.                    
009900     EJECT                                                                
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100   03 CBLTDLI            PIC X(8)    VALUE 'CBLTDLI '.                    
010200   03 FELLOG             PIC X(8)    VALUE 'FELLOG  '.                    
010300   03 W005INIT           PIC X(8)    VALUE 'W005INIT'.                    
010310   03 WMEDKONV           PIC X(8)    VALUE 'WMEDKONV'.                    
010400                                                                          
010410*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010420*01 -COPY WMEDAREA                                                        
010430     SKIP3                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010600*01 -COPY WMSGINIT                                                        
010700     EJECT                                                                
010710 01  MESSAGE-CODES.                                                       
010720     03  INF-REFILL-PART        PIC X(3)    VALUE '434'.                  
010730     EJECT                                                                
010800 01  MEDDELANDE.                                                          
010900                                                                          
011000* FEL-MEDDELANDEN                                                         
011100   03 W-FEL-1.                                                            
011200     05 FILLER  PIC X(26) VALUE 'ARTIKELNUMMER EJ NUMERISKT'.             
011300     05 FILLER  PIC X(26) VALUE 'PART NO. NOT NUMERIC      '.             
011400   03 FILLER REDEFINES W-FEL-1.                                           
011500     05 FEL-1   PIC X(26) OCCURS 2.                                       
011600                                                                          
011700   03 W-FEL-2.                                                            
011800     05 FILLER  PIC X(24) VALUE 'ARTIKEL SAKNAS I DATABAS'.               
011900     05 FILLER  PIC X(24) VALUE 'PART NO. MISSING IN BASE'.               
012000   03 FILLER REDEFINES W-FEL-2.                                           
012100     05 FEL-2   PIC X(24) OCCURS 2.                                       
012200                                                                          
012300   03 W-FEL-3.                                                            
012400     05 FILLER  PIC X(15) VALUE 'ARTIKELN ERSATT'.                        
012500     05 FILLER  PIC X(15) VALUE 'PART REPLACED  '.                        
012600   03 FILLER REDEFINES W-FEL-3.                                           
012700     05 FEL-3   PIC X(15) OCCURS 2.                                       
012800                                                                          
012900   03 W-FEL-4.                                                            
013000     05 FILLER  PIC X(19) VALUE 'ARTIKELN HAR UTGÅTT'.                    
013100     05 FILLER  PIC X(19) VALUE 'PART EXPIRED       '.                    
013200   03 FILLER REDEFINES W-FEL-4.                                           
013300     05 FEL-4   PIC X(19) OCCURS 2.                                       
013400                                                                          
013500   03 W-FEL-5.                                                            
013600     05 FILLER  PIC X(19) VALUE 'ERSÄTTANDE ARTIKEL'.                     
013700     05 FILLER  PIC X(19) VALUE 'REPLACING PART NO.'.                     
013800   03 FILLER REDEFINES W-FEL-5.                                           
013900     05 FEL-5   PIC X(19) OCCURS 2.                                       
014000                                                                          
014100   03 W-FEL-6.                                                            
014200     05 FILLER  PIC X(19) VALUE 'OBEHÖRIG ANVÄNDARE '.                    
014300     05 FILLER  PIC X(19) VALUE 'USER NOT AUTHORIZED'.                    
014400   03 FILLER REDEFINES W-FEL-6.                                           
014500     05 FEL-6   PIC X(19) OCCURS 2.                                       
014600                                                                          
014700* INFO-MEDDELANDEN                                                        
014800   03 W-MED-1.                                                            
014900     05 FILLER PIC X(32) VALUE 'FÖR MER INFORMATION TRYCK ENTER '.        
015000     05 FILLER PIC X(32) VALUE 'FOR MORE INFORMATION PRESS ENTER'.        
015100   03 FILLER REDEFINES W-MED-1.                                           
015200     05 MED-1  PIC X(32) OCCURS 2.                                        
015300                                                                          
015400   03 W-MED-2.                                                            
015500     05 FILLER PIC X(21) VALUE 'GEMENSAM PV/LV       '.                   
015600     05 FILLER PIC X(21) VALUE 'COMMON VCC/VTC       '.                   
015700   03 FILLER REDEFINES W-MED-2.                                           
015800     05 MED-2  PIC X(21) OCCURS 2.                                        
015900                                                                          
016000   03 W-MED-3.                                                            
016100     05 FILLER PIC X(46)              VALUE 'GEMENSAM PV/LV               
016200-                                     'FÖR MER INFO, TRYCK ENTER'.        
016300     05 FILLER PIC X(46)               VALUE 'COMMON VCC/VTC              
016400-                                    'FOR MORE INFO, PRESS ENTER'.        
016500   03 FILLER REDEFINES W-MED-3.                                           
016600     05 MED-3  PIC X(46) OCCURS 2.                                        
016700                                                                          
016800     EJECT                                                                
016900*                        ****    TP-AREOR                                 
017000 01      TP-WS.                                                           
017100   03    FILLER          PIC X(16)   VALUE '   TP-AREOR    '.             
017200     SKIP3                                                                
017300*01      MID -COPY W2I10101 -PRE MID-.                                    
017400     SKIP3                                                                
017500*01      -COPY WMSGAREA                                                   
017600     SKIP3                                                                
017700*  03    MOD -COPY W2O10101 -PRE MOD- -RED MSG-AREA.                      
017800     EJECT                                                                
017900*01  -COPY WMFSAREA.                                                      
018000     EJECT                                                                
018010******************************************************************        
018020*                                                                         
018030*        ARBETS-AREOR TILL DB2-SEKTIONERNA                                
018040*                                                                         
018050 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
018060       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018070                                                                          
018080 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
018090 01  DB2-WS.                                                              
018091     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
018092         88  CURSOR-OK                      VALUE 000.                    
018093         88  LINES-FOUND                    VALUE 000.                    
018094         88  LINES-MISSING                  VALUE 100.                    
018096         88  RESOURCE-WRONG                 VALUE 904.                    
018097     03  GOOD-SQLCODECODES.                                               
018098         05  GOOD-SQLCODE OCCURS 5                                        
018099             INDEXED BY SQLCODE-IX PIC 9(3).                              
018100     EJECT                                                                
018101******************************************************************        
018110******************************************************************        
018200*****                                                                     
018300*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018400*****                                                                     
018500 01  IMS-WS.                                                              
018600   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
018700     SKIP3                                                                
018800*****                    **** STATUS-KOD FRÅN IMS                         
018900   03    STATUS-WS       PIC XX.                                          
019000         88  SEGMENT-FINNS       VALUE '  '.                              
019100         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
019200     SKIP3                                                                
019300   03    GODK-STATUSKODER.                                                
019400     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
019500     SKIP3                                                                
019600 01      SSA1            PIC X(64).                                       
019700 01      SSA2            PIC X(64).                                       
019800 01      SSA3            PIC X(32).                                       
019900     SKIP3                                                                
020000*                            IMS FUNKTIONSKODER                           
020100*01      -COPY W0003                                                      
020200     EJECT                                                                
020300*                            DLI INPUT-OUTPUT AREA                        
020310 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK601'.         
020320 01  DLI-IO-WDK601.                                                       
020330*    03 -COPY WDK601                                                      
020340     EJECT                                                                
020350 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK611'.         
020360 01  DLI-IO-WDK611.                                                       
020370*    03 -COPY WDK611                                                      
020380     EJECT                                                                
021200 01      DLI-IO-AREA     PIC X(200)  VALUE SPACE.                         
021300     SKIP3                                                                
021400*01      WLARTC25 -COPY WDK625 -RED DLI-IO-AREA.                          
021500     EJECT                                                                
021600*01      WLKATN01 -COPY WDN601 -PRE KATN-     -RED DLI-IO-AREA.           
021700     EJECT                                                                
021800*01      WLKATN11 -COPY WDN611 -PRE KATN-     -RED DLI-IO-AREA.           
021900     EJECT                                                                
022000*01      WLBENA01 -COPY WDD301 -PRE BENA01-  -RED DLI-IO-AREA.            
022100     EJECT                                                                
022200*01      WLBENA11 -COPY WDD311 -PRE BENA11-  -RED DLI-IO-AREA.            
022201     EJECT                                                                
022210 01  FILLER              PIC X(16) VALUE 'DLI-IO-WDGX1144'.               
022220 01  DLI-IO-WDGX1144.                                                     
022230*    03  -COPY WDGX1144                                                   
022240                                                                          
022300     EJECT                                                                
022310*    --------------- DB2 INPUT-OUTPUT AREA ---------------                
022320                                                                          
022392 01  FILLER                      PIC X(16)  VALUE 'TB1ACCE-AREA'.         
022393                                                                          
022394*01  -COPY TB1ACCE -PRE TB1ACCE-                                          
022395     EJECT                                                                
022400     EXEC SQL INCLUDE TB1ACCE END-EXEC.                                   
022401     EJECT                                                                
022410 LINKAGE SECTION.                                                         
022500*01  -COPY W0009     -PRE MSG-                                            
022600     SKIP3                                                                
022700*01  -COPY W0008     -PRE AA-                                             
022800         05  FILLER           PIC X.                                      
022900     SKIP3                                                                
023000*01  -COPY W0008     -PRE USEA-                                           
023100         05  FILLER           PIC X.                                      
023200     SKIP3                                                                
023300*01  -COPY W0008     -PRE AB-                                             
023400         05  FILLER           PIC X.                                      
023500*01  -COPY W0008     -PRE BENC-                                           
023600         05  FILLER           PIC X.                                      
023700*01  -COPY W0008     -PRE KATN-                                           
023800         05  FILLER           PIC X.                                      
023810*01  -COPY W0008     -PRE WDG2-                                           
023820         05  FILLER           PIC X.                                      
023900     EJECT                                                                
024000 PROCEDURE DIVISION USING MSG-PCB USEA-PCB AB-PCB                         
024100                          BENC-PCB KATN-PCB WDG2-PCB.                     
024200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB AB-PCB                        
024300                          BENC-PCB KATN-PCB WDG2-PCB.                     
024400                                                                          
024500     PERFORM IMS-GET-MSG                                                  
024600                                                                          
024700     IF  SEGMENT-FINNS                                                    
024800         PERFORM A-KONTROLL-NYCKLAR-OCH-INIT                              
024900                                                                          
025000         IF  SW-NYCKLAR-OK = JA                                           
025100                                                                          
025200             MOVE IDARTNR-WS TO W-IDARTNR                                 
025300             PERFORM IMS-GET-ART-SEG                                      
025400                                                                          
025500             IF  SEGMENT-FINNS                                            
025600                 MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                
025700                 IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                
025800                 OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE          
025900*                  --- BEHÖRIG USER                                       
026000                     MOVE ART-KDERS-UTG TO MOD-KDERS-UTG                  
026100                     MOVE ART-REKSIFFR TO MOD-REKSIFFR                    
026200                     MOVE STRECK TO MOD-STRECK                            
026300                     MOVE ART-TIFINLV TO MOD-TIFINLV                      
026400**                   MOVE ART-IDLEVNR   TO MOD-IDLEVNR                    
026500                     MOVE ART-IDFKNGRP  TO MOD-IDFKNGRP                   
026600                     MOVE ART-KDSORT    TO MOD-KDSORT                     
026700                     MOVE ART-TIURPROD  TO MOD-TIURPROD                   
026800                                                                          
026810                     PERFORM F-RED-MOD-KVAARLF                            
026811                     PERFORM G-RED-MOD-TB1ACCE                            
026820                                                                          
026900                     IF ART-KDERS-UTG > ZERO                              
027000                         IF ART-KDERS-UTG = +29 OR +52                    
027100                             MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL           
027200                         ELSE                                             
027300                             MOVE FEL-3 (SPIND) TO MOD-TEMFSFEL           
027400                         END-IF                                           
027500                     ELSE                                                 
027600                         IF ART-FLERS = JA                                
027700                             MOVE FEL-5 (SPIND) TO MOD-TEMFSINF           
027800                         END-IF                                           
027900                         PERFORM B-RED-BILD-FRAN-WDK6                     
028000                         PERFORM D-RED-BILD-FRAN-WDD3                     
028100                         PERFORM E-RED-BILD-FRAN-WDN6                     
028200                     END-IF                                               
028300                 ELSE                                                     
028400*                    -- EJ BEHÖRIG USER                                   
028500                     MOVE FEL-6 (SPIND) TO MOD-TEMFSFEL                   
028600                 END-IF                                                   
028700             ELSE                                                         
028800                MOVE FEL-2 (SPIND) TO MOD-TEMFSFEL                        
028900             END-IF                                                       
029000         END-IF                                                           
029100     SKIP1                                                                
029200         PERFORM IMS-INSERT-MSG                                           
029300     END-IF                                                               
029400     SKIP1                                                                
029500     MOVE ZERO TO RETURN-CODE                                             
029600     GOBACK.                                                              
029700     EJECT                                                                
029800 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
029900******************************************************************        
030000*                                                                *        
030100*                                                                *        
030200*                                                                *        
030300******************************************************************        
030400     SKIP1                                                                
030500     MOVE JA TO SW-NYCKLAR-OK                                             
030600     IF MSG-DUBBLA-TRANSKODER                                             
030700         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
030800         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
030900         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10101               
031000         MOVE ZERO TO LASTA-SEG                                           
031100     ELSE                                                                 
031200         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
031300         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
031400         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I10101                
031500     END-IF                                                               
031600                                                                          
031700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
031800     MOVE '001'             TO MSGI-KDCALL                                
031900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
032100     MOVE '2101'            TO MSGI-IDTRANS                               
032200                                                                          
032300     IF MFS-IDTRANS = '2101'                                              
032400     OR (MID-IDARTNR-IN NUMERIC                                           
032500     AND MID-IDARTNR-IN > ZERO)                                           
032600        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
032800        MOVE ZERO           TO LASTA-SEG                                  
032801     ELSE                                                                 
032802        MOVE ZERO           TO LASTA-SEG                                  
032810        MOVE ZERO           TO MID-BLADDRING-ANT                          
032900     END-IF                                                               
033000                                                                          
033100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033200                                                                          
033300     MOVE MSGI-IDARTNR   TO IDARTNR-WS                                    
033400                            MOD-IDARTNR-UT                                
033500     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
033600                                                                          
033700     IF MSGI-IDLAND-SPR = SPACE                                           
033800       IF MSGI-IDSPRAK = SPACE                                            
033900         IF SWEDISH-TEXT                                                  
034000           MOVE +1 TO SPIND                                               
034100         ELSE                                                             
034200           MOVE +2 TO SPIND                                               
034300         END-IF                                                           
034400       ELSE                                                               
034500         IF MSGI-IDSPRAK = 'SV'                                           
034600           MOVE +1 TO SPIND                                               
034700         ELSE                                                             
034800           MOVE +2 TO SPIND                                               
034900         END-IF                                                           
035000       END-IF                                                             
035100     ELSE                                                                 
035200       IF MSGI-IDLAND-SPR = 'SE'                                          
035300         MOVE +1 TO SPIND                                                 
035400       ELSE                                                               
035500         MOVE +2 TO SPIND                                                 
035600       END-IF                                                             
035700     END-IF                                                               
035800                                                                          
035900     SKIP2                                                                
036000     MOVE LOW-VALUE TO MOD-AREA                                           
036100     MOVE 'W2O10101' TO MFS-IDMOD                                         
036200     MOVE '2101' TO MOD-IDTRANS                                           
036300     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
036400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
036500                      MOD-TEMFSFEL                                        
036600                      MOD-BEART-SVE                                       
036700                      MOD-IDKAT (1)                                       
036800                      MOD-IDKAT (2)                                       
036900                      MOD-IDKAT (3)                                       
037000                      MOD-IDKAT (4)                                       
037100                      MOD-IDKAT (5)                                       
037200                      MOD-IDKAT (6)                                       
037300                      MOD-IDKAT (7)                                       
037400                      MOD-IDKAT (8)                                       
037500                      MOD-IDKAT (9)                                       
037600                      MOD-IDKAT (10)                                      
037700                      MOD-IDKAT (11)                                      
037800                      MOD-IDKAT-1                                         
037900                      MOD-IDKAT-2                                         
038000                      MOD-IDKAT-3                                         
038100                      MOD-IDAO (1)                                        
038200                      MOD-IDAO (2)                                        
038300                      MOD-IDAO (3)                                        
038400                      MOD-IDAO (4)                                        
038500                      MOD-IDAO (5)                                        
038600                      MOD-VARNOT                                          
038700                      MOD-TEARTNOT (1)                                    
038800                      MOD-TEARTNOT (2)                                    
038810                      MOD-BEUPPDSU                                        
038900                      MOD-TEMFSINF                                        
039000     SKIP1                                                                
039200     IF  MID-BLADDRING-ANT NOT NUMERIC                                    
039300     OR  MFS-IDTRANS NOT = '2101'                                         
039400         MOVE ZERO TO MID-BLADDRING-ANT                                   
039500     END-IF                                                               
039600     SKIP1                                                                
039700                                                                          
039800     IF  IDARTNR-WS NOT NUMERIC                                           
039900         MOVE FEL-1 (SPIND)    TO MOD-TEMFSFEL                            
040000         MOVE NEJ TO SW-NYCKLAR-OK                                        
040100     END-IF.                                                              
040200                                                                          
040300     EJECT                                                                
040400 B-RED-BILD-FRAN-WDK6 SECTION.                                            
040500******************************************************************        
040600*                                                                *        
040700*    REDIGERING AV BILD MED DATA FRÅN WDK6                       *        
040800*                                                                *        
040900******************************************************************        
041000                                                                          
041100     MOVE +1       TO IX                                                  
041200     SET MOD-IX    TO 1                                                   
041300     PERFORM UNTIL IX > MAX-ANT-AO-NUMMER                                 
041400         MOVE ART-IDAO(IX) TO MOD-IDAO(MOD-IX)                            
041500         ADD 1 TO IX                                                      
041600         SET MOD-IX UP BY 1                                               
041700     END-PERFORM                                                          
041800     SKIP3                                                                
041900     PERFORM IMS-GET-CLAG-SEG                                             
042000     IF  SEGMENT-FINNS                                                    
042100         MOVE CLAG-IDBERED TO MOD-IDBERED                                 
042200         MOVE CLAG-IDKAT(1) TO MOD-IDKAT-1                                
042300         MOVE CLAG-IDKAT(2) TO MOD-IDKAT-2                                
042400         MOVE CLAG-IDKAT(3) TO MOD-IDKAT-3                                
042500         MOVE CLAG-KDSEGKEY TO W-KDSEGKEY                                 
042600     END-IF                                                               
042700                                                                          
042800     MOVE 1 TO IX                                                         
042900     PERFORM UNTIL (IX > MAX-ANT-PROENH)                                  
043000         MOVE   CLAG-IDPROENH (IX) TO MOD-IDPROENH (IX)                   
043100         INSPECT MOD-IDPROENH (IX) REPLACING LEADING ZERO BY SPACE        
043200         ADD 1 TO IX                                                      
043300     END-PERFORM                                                          
043400     SKIP3                                                                
043500         PERFORM BA-RED-FRAN-MATINFO-WDK611                               
043600     SKIP3                                                                
043700         MOVE SPACE         TO MOD-FLFSP (1)                              
043800     SKIP3                                                                
043900* ÄT880919 ****************************************************           
044000*                                                                         
044100     MOVE 1                   TO W-KDNOTTYP                               
044200     PERFORM IMS-GET-NOT-SEG                                              
044300                                                                          
044400     IF SEGMENT-FINNS                                                     
044500        MOVE NOT-TEARTNOT     TO MOD-TEARTNOT (1)                         
044600     ELSE                                                                 
044700        MOVE MFS-RENSA-FAELT  TO MOD-TEARTNOT (1)                         
044800     END-IF                                                               
044900                                                                          
045000     MOVE 2                   TO W-KDNOTTYP                               
045100     PERFORM IMS-GET-NOT-SEG                                              
045200                                                                          
045300     IF SEGMENT-FINNS                                                     
045400        MOVE NOT-TEARTNOT     TO MOD-TEARTNOT (2)                         
045500     ELSE                                                                 
045600        MOVE MFS-RENSA-FAELT  TO MOD-TEARTNOT (2)                         
045700     END-IF                                                               
045800                                                                          
045900     MOVE 6                   TO W-KDNOTTYP                               
046000     PERFORM IMS-GET-NOT-SEG                                              
046100                                                                          
046200     IF SEGMENT-FINNS                                                     
046300        MOVE NOT-TEARTNOT     TO MOD-VARNOT                               
046400     ELSE                                                                 
046500        MOVE MFS-RENSA-FAELT  TO MOD-VARNOT                               
046600     END-IF                                                               
046700                                                                          
046800*-----------------------------------------------------------------        
046900     SKIP3                                                                
047000*  BORTTAG AV KTO INFÖR SAP 980901  GS  **                                
047100*        MOVE CLAG-IDLKTO      TO W-IDLKTO-N                              
047200*        MOVE W-IDLKTO-3-5     TO W-IDLKTO-3-5-FLYTT                      
047300*        MOVE W-IDLKTO-6-7     TO W-IDLKTO-6-7-FLYTT                      
047400*        MOVE STRECK           TO FILLER-STRECK                           
047500*        MOVE W-IDLKTO-FLYTT-N TO MOD-IDLKTO-3-7                          
047600********************************************                              
047700     SKIP3                                                                
047800         PERFORM BB-RED-FRAN-GEMINFO-WDK611                               
047900     .                                                                    
048000     EJECT                                                                
048100 BA-RED-FRAN-MATINFO-WDK611 SECTION.                                      
048200     SKIP3                                                                
048300     MOVE CLAG-FLJIT           TO MOD-FLJIT                               
048400     MOVE CLAG-IDANSK          TO MOD-IDANSK                              
048500     MOVE CLAG-IDINK           TO MOD-IDINK                               
048600     MOVE CLAG-IDPLANGR-AG     TO MOD-IDPLANGR-AG                         
048700     MOVE CLAG-IDPLANGR-LEV TO MOD-IDPLANGR-LEV                           
048800     SKIP1                                                                
048900     MOVE CLAG-KDAVT          TO MOD-KDAVT                                
049000     MOVE CLAG-KDHF           TO MOD-KDHF                                 
049100     MOVE CLAG-KDKSP          TO MOD-KDKSP                                
049200     MOVE CLAG-KDVVKL         TO MOD-KDVVKL                               
049300     MOVE CLAG-IDPROJ         TO MOD-IDPROJ                               
049400     SKIP1                                                                
049500     MOVE ZERO                TO MOD-KVAL                                 
049600     MOVE CLAG-KVAP           TO MOD-KVAP                                 
049700     MOVE CLAG-KVULOAD        TO MOD-KVULOAD                              
049800     MOVE CLAG-FLCDART        TO MOD-FLCDART                              
049900     MOVE CLAG-KVBK           TO MOD-KVBK                                 
050000     MOVE CLAG-KVDAGAR-INLEV TO MOD-KVDAGAR-INLEV                         
050100     MOVE CLAG-KVDAGAR-TT     TO MOD-KVDAGAR-TT                           
050200     MOVE CLAG-KVKP           TO MOD-KVKP                                 
050300     MOVE CLAG-KVOVERF        TO MOD-KVOVERF                              
050400     MOVE CLAG-KVPALL         TO MOD-KVPALL                               
050500     MOVE CLAG-KVQ            TO MOD-KVQ                                  
050600     MOVE CLAG-KVQ-JUST       TO MOD-KVQ-JUST                             
050700     MOVE CLAG-TIQJUST        TO MOD-TIQJUST                              
050800     MOVE CLAG-KVVECKOR-AT    TO MOD-KVVECKOR-AT                          
050900     MOVE CLAG-KVVECKOR-BT    TO MOD-KVVECKOR-BT                          
051000     MOVE CLAG-KVVECKOR-FT    TO MOD-KVVECKOR-FT                          
051100     MOVE CLAG-KVVECKOR-LT    TO MOD-KVVECKOR-LT                          
051200     SKIP1                                                                
051300     MOVE  CLAG-FLAVRART      TO MOD-FLAVRART                             
051400     MOVE  CLAG-FLMANAT       TO MOD-FLMANAT                              
051500     MOVE  CLAG-FLMANBK       TO MOD-FLMANBK                              
051600     MOVE  CLAG-FLMANKP       TO MOD-FLMANKP                              
051700     MOVE  CLAG-FLMANLT       TO MOD-FLMANLT                              
051800     MOVE  CLAG-FLMANQ        TO MOD-FLMANQ                               
051810     MOVE CLAG-IDLEVNR-SHIP   TO MOD-IDLEVNR-SHIP                         
051900     IF CLAG-FLGEMART = JA                                                
052000        MOVE JA TO WS-FLGEMART                                            
052100        MOVE MED-2 (SPIND)    TO MOD-TEMFSINF                             
052200     END-IF                                                               
052220                                                                          
052230     IF CLAG-IDDC-REF NOT = SPACE                                         
052240        MOVE INF-REFILL-PART  TO MED-IDMFSFEL                             
052250        CALL WMEDKONV      USING MED-WMEDAREA                             
052260        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
052270     END-IF                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 BB-RED-FRAN-GEMINFO-WDK611 SECTION.                                      
052600     SKIP3                                                                
052700     SKIP1                                                                
052800*    MOVE CLAG-KDGK TO MOD-KDGK                                           
052900*    MOVE CLAG-KDLTK TO MOD-KDLTK                                         
053000     MOVE CLAG-KVEOQ TO MOD-KVEOQ                                         
053100     SKIP1                                                                
053200     MOVE CLAG-KDLEVSP TO W-KDLEVSP-N                                     
053300     MOVE NEJ TO MOD-KDLEVSP-C1                                           
053400     MOVE SPACE TO MOD-KDLEVSP-C2                                         
053500     IF  CLAG-KDLEVSP = 20                                                
053600         MOVE JA TO MOD-KDLEVSP-C1                                        
053700     ELSE                                                                 
053800         IF  W-KDLEVSP-2 = 1                                              
053900             MOVE JA TO MOD-KDLEVSP-C1                                    
054000         END-IF                                                           
054100     END-IF                                                               
054200     SKIP1                                                                
054300*    MOVE  CLAG-FLMANGK       TO MOD-FLMANGK                              
054400     MOVE  CLAG-KDUART        TO MOD-KDUART                               
054500     MOVE  CLAG-FLLSRDEL      TO MOD-FLLSRDEL                             
054600     MOVE  CLAG-FLLTKSP       TO MOD-FLLTKSP                              
054700     MOVE  CLAG-KDARTURS      TO MOD-KDARTURS                             
054800     MOVE  CLAG-KDFARLIG      TO MOD-KDFARLIG                             
054900     MOVE  CLAG-KDSRA         TO MOD-KDSRA                                
055000     .                                                                    
055100     EJECT                                                                
055200 D-RED-BILD-FRAN-WDD3 SECTION.                                            
055300******************************************************************        
055400*                                                                *        
055500*    REDIGERING AV BILD MED DATA FRÅN WDD3                       *        
055600*                                                                *        
055700******************************************************************        
055800*ÄT851015                                                                 
055900     SKIP1                                                                
056000     PERFORM IMS-GET-BENA01-CSEQ                                          
056100     IF MSGI-IDLAND-SPR = 'GB'                                            
056200        MOVE 'GB '          TO W-IDSKYLT                                  
056300     ELSE                                                                 
056400        MOVE 'S  '          TO W-IDSKYLT                                  
056500     END-IF                                                               
056600     PERFORM IMS-GET-BENA11-CSEQ                                          
056700     MOVE BENA11-TEXT-BEART TO MOD-BEART-SVE.                             
056800     EJECT                                                                
056900 E-RED-BILD-FRAN-WDN6 SECTION.                                            
057000******************************************************************        
057100*                                                                *        
057200*    REDIGERING AV BILD MED DATA FRÅN WDN6                       *        
057300*                                                                *        
057400******************************************************************        
057500*ÄT861217                                                                 
057600     SKIP1                                                                
057700     PERFORM IMS-GET-MASTER-WDN6                                          
057800     IF SEGMENT-FINNS                                                     
057900        PERFORM IMS-GET-KATINFO-MASTER                                    
058000        IF MID-BLADDRING-ANT = ZERO                                       
058100           CONTINUE                                                       
058200        ELSE                                                              
058300           SET MOD-IZ    TO +1                                            
058400           MOVE MID-BLADDRING-ANT TO LASTA-SEG                            
058500           PERFORM UNTIL MOD-IZ > LASTA-SEG OR SEGMENT-SAKNAS             
058600              PERFORM IMS-GET-KATINFO-MASTER                              
058700              SET MOD-IZ  UP BY +1                                        
058800           END-PERFORM                                                    
058900        END-IF                                                            
059000        SET MOD-IZ     TO +1                                              
059100        PERFORM UNTIL MOD-IZ > 11 OR SEGMENT-SAKNAS                       
059200           MOVE KATN-KAT-BEEMBLEM TO MOD-IDKAT (MOD-IZ)                   
059300           ADD +1        TO LASTA-SEG                                     
059400           PERFORM IMS-GET-KATINFO-MASTER                                 
059500           SET MOD-IZ UP BY +1                                            
059600        END-PERFORM                                                       
059700        IF SEGMENT-FINNS                                                  
059800           MOVE LASTA-SEG  TO MOD-BLADDRING-ANT                           
059900           IF WS-FLGEMART = JA                                            
060000              MOVE MED-3 (SPIND) TO MOD-TEMFSINF                          
060100           ELSE                                                           
060200              MOVE MED-1 (SPIND) TO MOD-TEMFSINF                          
060300           END-IF                                                         
060400        ELSE                                                              
060500           MOVE ZERO       TO MOD-BLADDRING-ANT                           
060600        END-IF                                                            
060700     END-IF.                                                              
060800                                                                          
060810 F-RED-MOD-KVAARLF SECTION.                                               
060811                                                                          
060812     MOVE ART-IDFKNGRP            TO W-IDFKNGRP-FOM                       
060813                                     W-IDFKNGRP-TOM                       
060818     PERFORM IMS-GU-WDGX1144                                              
060819     IF SEGMENT-FINNS                                                     
060820        MOVE 1144-KVAARLF         TO MOD-KVAARLF                          
060823     ELSE                                                                 
060824        MOVE 15                   TO MOD-KVAARLF                          
060825     END-IF                                                               
060828     .                                                                    
060830                                                                          
060840 G-RED-MOD-TB1ACCE SECTION.                                               
060850                                                                          
060896     PERFORM DB2-SELECT-TB1ACCE-TAB                                       
060897     IF LINES-FOUND                                                       
060898       MOVE TB1ACCE-BEUPPDSU        TO MOD-BEUPPDSU                       
060899     ELSE                                                                 
060900       MOVE SPACES                  TO MOD-BEUPPDSU                       
060901     END-IF                                                               
060902     .                                                                    
060903                                                                          
060904                                                                          
060910* IMS SEKTIONER                                                           
061000     SKIP3                                                                
061100 IMS-GET-MSG SECTION.                                                     
061200     MOVE '  QC' TO GODK-STATUSKODER                                      
061300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061500     PERFORM IMS-STATUSKONTROLL.                                          
061600     SKIP3                                                                
061700 IMS-INSERT-MSG SECTION.                                                  
061800     IF MSGI-IDLAND-SPR = 'GB'                                            
061900        MOVE 'N' TO MFS-KDHUVOMR                                          
062000     END-IF                                                               
062100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
062200     MOVE SPACE TO GODK-STATUSKODER                                       
062300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
062400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062500     PERFORM IMS-STATUSKONTROLL.                                          
062600     EJECT                                                                
062700 IMS-GET-ART-SEG SECTION.                                                 
062800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
062900            DELIMITED BY SIZE INTO SSA1                                   
063000     MOVE '  GE' TO GODK-STATUSKODER                                      
063100     CALL CBLTDLI USING GU AB-PCB DLI-IO-WDK601 SSA1                      
063200     MOVE AB-STATUS-CODE TO STATUS-WS                                     
063300     PERFORM IMS-STATUSKONTROLL.                                          
063400     SKIP3                                                                
063500 IMS-GET-CLAG-SEG SECTION.                                                
063600     MOVE 'WLARTC11' TO SSA1                                              
063700     MOVE '  GE' TO GODK-STATUSKODER                                      
063800     CALL CBLTDLI USING GNP AB-PCB DLI-IO-WDK611 SSA1                     
063900     MOVE AB-STATUS-CODE TO STATUS-WS                                     
064000     PERFORM IMS-STATUSKONTROLL.                                          
064100     SKIP3                                                                
064200 IMS-GET-NOT-SEG SECTION.                                                 
064300     MOVE 'WLARTC11*F' TO SSA1                                            
064400     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
064500            DELIMITED BY SIZE INTO SSA2                                   
064600     MOVE '  GE' TO GODK-STATUSKODER                                      
064700     CALL CBLTDLI USING GNP AB-PCB DLI-IO-AREA SSA1 SSA2                  
064800     MOVE AB-STATUS-CODE TO STATUS-WS                                     
064900     PERFORM IMS-STATUSKONTROLL.                                          
065000     EJECT                                                                
065100 IMS-GET-BENA01-CSEQ SECTION.                                             
065200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
065300             DELIMITED BY SIZE INTO SSA1                                  
065400     MOVE '  ' TO GODK-STATUSKODER                                        
065500     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA SSA1                      
065600     MOVE BENC-STATUS-CODE  TO STATUS-WS                                  
065700     PERFORM IMS-STATUSKONTROLL.                                          
065800     SKIP3                                                                
065900 IMS-GET-BENA11-CSEQ SECTION.                                             
066000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
066100             DELIMITED BY SIZE INTO SSA1                                  
066200     MOVE '  ' TO GODK-STATUSKODER                                        
066300     CALL CBLTDLI USING GNP BENC-PCB DLI-IO-AREA SSA1                     
066400     MOVE BENC-STATUS-CODE  TO STATUS-WS                                  
066500     PERFORM IMS-STATUSKONTROLL.                                          
066600     EJECT                                                                
066700 IMS-GET-MASTER-WDN6 SECTION.                                             
066800     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
066900             DELIMITED BY SIZE INTO SSA1                                  
067000     MOVE '  GE' TO GODK-STATUSKODER                                      
067100     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA SSA1                      
067200     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
067300     PERFORM IMS-STATUSKONTROLL.                                          
067400     SKIP3                                                                
067500 IMS-GET-KATINFO-MASTER SECTION.                                          
067600     MOVE 'WLKATN11 ' TO SSA1                                             
067700     MOVE '  GE' TO GODK-STATUSKODER                                      
067800     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA SSA1                     
067900     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
068000     PERFORM IMS-STATUSKONTROLL.                                          
068100                                                                          
068223 IMS-GU-WDGX1144 SECTION.                                                 
068224                                                                          
068225     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
068226            DELIMITED BY SIZE INTO SSA1                                   
068227     STRING 'WDGX1144(IDFKNGRF=<' W-IDFKNGRP-FOM-X                        
068228                    '&IDFKNGRT=>' W-IDFKNGRP-TOM-X ')'                    
068229          DELIMITED BY SIZE  INTO SSA2                                    
068230     MOVE '  GE'               TO GODK-STATUSKODER                        
068231     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX1144 SSA1 SSA2             
068232     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
068233     PERFORM IMS-STATUSKONTROLL                                           
068234     .                                                                    
068235                                                                          
068240 IMS-STATUSKONTROLL SECTION.                                              
068300     SET STATUS-IX TO 1                                                   
068400     SEARCH GODK-STATUS AT END CALL FELLOG                                
068500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
068600     END-SEARCH.                                                          
068610                                                                          
068700 DB2-SELECT-TB1ACCE-TAB  SECTION.                                         
068800     SKIP2                                                                
068900     MOVE 'DB2-SELECT-TB1ACCE-TAB       ' TO WS-DB2-SEKTION               
069000     MOVE 000100  TO GOOD-SQLCODECODES                                    
069100     EXEC SQL                                                             
069600                                                                          
069620       SELECT BEUPPDSU                                                    
069630       INTO                                                               
069650          :TB1ACCE-BEUPPDSU                                               
069700       FROM   TB1ACCE                                                     
069800                                                                          
069900       WHERE   IDARTNR = :IDARTNR-WS                                      
069910       ORDER BY TIAOINF DESC                                              
069920       FETCH FIRST 1 ROW ONLY                                             
070000                                                                          
070100     END-EXEC                                                             
070200                                                                          
070300     MOVE SQLCODE TO SQLCODE-WS                                           
070400     PERFORM DB2-STATUS-CHECK                                             
070500     .                                                                    
070600     EJECT                                                                
070700 DB2-STATUS-CHECK  SECTION.                                               
070800                                                                          
070900     SET SQLCODE-IX TO 1                                                  
071000     SEARCH GOOD-SQLCODE                                                  
071100       AT END                                                             
071200*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
071300*         DELIMITED BY SIZE INTO ERROR-TEXT                               
071400          CALL FELLOG                                                     
071500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
071600     END-SEARCH                                                           
071700     .                                                                    
071800     EJECT                                                                
