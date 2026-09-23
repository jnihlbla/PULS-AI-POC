000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                W9042200.                                     
000300 AUTHOR.                    IDK, GÖTEBORG.                                
000400 DATE-COMPILED.                                                           
000500 DATE-WRITTEN.              APRIL -79.                                    
000600     REMARKS.                                                             
000700*    FUNKTION.   TP-PROGRAM. FRÅGE-PROGRAM SOM ANGER                      
000800*                'ARTIKELINFO - ANSK'                                     
000900     INDATA.                                                              
001000         TRANSAKTION: W90422T                                             
001100         MID:         W90422I1                                            
001200     UTDATA.                                                              
001300         MOD:         W90422O1                                            
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
003000 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
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
010400                                                                          
010500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010600*01 -COPY WMSGINIT                                                        
010700     EJECT                                                                
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
017300*01      MID -COPY W90422I1 -PRE MID-.                                    
017400     SKIP3                                                                
017500*01      -COPY WMSGAREA                                                   
017600     SKIP3                                                                
017700*  03    MOD -COPY W90422O1 -PRE MOD- -RED MSG-AREA.                      
017800     EJECT                                                                
017900*01  -COPY WMFSAREA.                                                      
018000     EJECT                                                                
018100******************************************************************        
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
019600 01      SSA1            PIC X(32).                                       
019700 01      SSA2            PIC X(32).                                       
019800 01      SSA3            PIC X(32).                                       
019900     SKIP3                                                                
020000*                            IMS FUNKTIONSKODER                           
020100*01      -COPY W0003                                                      
020200     EJECT                                                                
020300*                            DLI INPUT-OUTPUT AREA                        
020400 01  DLI-IO-AREA-01.                                                      
      *    03  -COPY WDK601                                                     
020700     EJECT                                                                
       01  DLI-IO-AREA-11.                                                      
      *    03 -COPY WDK611                                                      
021100     EJECT                                                                
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
022300     EJECT                                                                
022400 LINKAGE SECTION.                                                         
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
023900     EJECT                                                                
024000 PROCEDURE DIVISION USING MSG-PCB USEA-PCB AB-PCB                         
024100                          BENC-PCB KATN-PCB.                              
024200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB AB-PCB                        
024300                           BENC-PCB KATN-PCB.                             
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
026000*                    MOVE ART-KDERS-UTG TO MOD-KDERS-UTG                  
026100*                    MOVE ART-REKSIFFR TO MOD-REKSIFFR                    
026200*                    MOVE STRECK TO MOD-STRECK                            
026300*                    MOVE ART-TIFINLV TO MOD-TIFINLV                      
026400*                    MOVE ART-IDLEVNR   TO MOD-IDLEVNR                    
026500*                    MOVE ART-IDFKNGRP  TO MOD-IDFKNGRP                   
026600*                    MOVE ART-KDSORT    TO MOD-KDSORT                     
026700                                                                          
026800                     IF ART-KDERS-UTG > ZERO                              
026900                         IF ART-KDERS-UTG = +29 OR +52                    
027000                             MOVE FEL-4 (SPIND) TO MOD-TEMFSFEL           
027100                         ELSE                                             
027200                             MOVE FEL-3 (SPIND) TO MOD-TEMFSFEL           
027300                         END-IF                                           
027400                     ELSE                                                 
027500                         IF ART-FLERS = JA                                
027600                             MOVE FEL-5 (SPIND) TO MOD-TEMFSINF           
027700                         END-IF                                           
027710                         MOVE ART-TIURPROD      TO MOD-TIURPROD           
027800                         PERFORM B-RED-BILD-FRAN-WDK6                     
027900                         PERFORM D-RED-BILD-FRAN-WDD3                     
028000                         PERFORM E-RED-BILD-FRAN-WDN6                     
028100                     END-IF                                               
028200                 ELSE                                                     
028300*                    -- EJ BEHÖRIG USER                                   
028400                     MOVE FEL-6 (SPIND) TO MOD-TEMFSFEL                   
028500                 END-IF                                                   
028600             ELSE                                                         
028700                MOVE FEL-2 (SPIND) TO MOD-TEMFSFEL                        
028800             END-IF                                                       
028900         END-IF                                                           
029000     SKIP1                                                                
029100         PERFORM IMS-INSERT-MSG                                           
029200     END-IF                                                               
029300     SKIP1                                                                
029400     MOVE ZERO TO RETURN-CODE                                             
029500     GOBACK.                                                              
029600     EJECT                                                                
029700 A-KONTROLL-NYCKLAR-OCH-INIT SECTION.                                     
029800******************************************************************        
029900*                                                                *        
030000*                                                                *        
030100*                                                                *        
030200******************************************************************        
030300     SKIP1                                                                
030400     MOVE JA TO SW-NYCKLAR-OK                                             
030500     IF MSG-DUBBLA-TRANSKODER                                             
030600         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
030700         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
030800         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90422I1-CTX           
030900         MOVE ZERO TO LASTA-SEG                                           
031000     ELSE                                                                 
031100         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
031200         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
031300         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90422I1-CTX            
031400     END-IF                                                               
031500                                                                          
031600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
031700     MOVE '001'             TO MSGI-KDCALL                                
031800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
031900                               MSGI-IDLTERM-USER                          
032000     MOVE '9422'            TO MSGI-IDTRANS                               
032100                                                                          
032200     IF MFS-IDTRANS = '9422'                                              
032300     OR (MID-IDARTNR-IN NUMERIC                                           
032400     AND MID-IDARTNR-IN > ZERO)                                           
032500        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
032600*       MOVE ZERO           TO MID-BLADDRING-ANT                          
032700                               LASTA-SEG                                  
032800     END-IF                                                               
032900                                                                          
033000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033100                                                                          
033200     MOVE MSGI-IDARTNR   TO IDARTNR-WS                                    
033300*                           MOD-IDARTNR-UT                                
033400*    INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
033500                                                                          
033600     IF MSGI-IDLAND-SPR = SPACE                                           
033700       IF MSGI-IDSPRAK = SPACE                                            
033800         IF SWEDISH-TEXT                                                  
033900           MOVE +1 TO SPIND                                               
034000         ELSE                                                             
034100           MOVE +2 TO SPIND                                               
034200         END-IF                                                           
034300       ELSE                                                               
034400         IF MSGI-IDSPRAK = 'SV'                                           
034500           MOVE +1 TO SPIND                                               
034600         ELSE                                                             
034700           MOVE +2 TO SPIND                                               
034800         END-IF                                                           
034900       END-IF                                                             
035000     ELSE                                                                 
035100       IF MSGI-IDLAND-SPR = 'SE'                                          
035200         MOVE +1 TO SPIND                                                 
035300       ELSE                                                               
035400         MOVE +2 TO SPIND                                                 
035500       END-IF                                                             
035600     END-IF                                                               
035700                                                                          
035800     SKIP2                                                                
035900     MOVE LOW-VALUE TO MOD-AREA                                           
036000     MOVE 'W90422O1' TO MFS-IDMOD                                         
036100     MOVE '9422' TO MOD-IDTRANS                                           
036200     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
036300     MOVE MFS-RENSA-FAELT TO                                              
036310*                     MOD-IDARTNR-IN                                      
036400                      MOD-TEMFSFEL                                        
036500*                     MOD-BEART-SVE                                       
036600*                     MOD-IDKAT (1)                                       
036700*                     MOD-IDKAT (2)                                       
036800*                     MOD-IDKAT (3)                                       
036900*                     MOD-IDKAT (4)                                       
037000*                     MOD-IDKAT (5)                                       
037100*                     MOD-IDKAT (6)                                       
037200*                     MOD-IDKAT (7)                                       
037300*                     MOD-IDKAT (8)                                       
037400*                     MOD-IDKAT (9)                                       
037500*                     MOD-IDKAT (10)                                      
037600*                     MOD-IDKAT (11)                                      
037700*                     MOD-IDKAT-1                                         
037800*                     MOD-IDKAT-2                                         
037900*                     MOD-IDKAT-3                                         
038000*                     MOD-IDAO (1)                                        
038100*                     MOD-IDAO (2)                                        
038200*                     MOD-IDAO (3)                                        
038300*                     MOD-IDAO (4)                                        
038400*                     MOD-IDAO (5)                                        
038500*                     MOD-VARNOT                                          
038600                      MOD-TEARTNOT (1)                                    
038700                      MOD-TEARTNOT (2)                                    
038800                      MOD-TEMFSINF                                        
038900     SKIP1                                                                
039000*    IF  MID-BLADDRING-ANT NOT NUMERIC                                    
039100*    OR  MFS-IDTRANS NOT = '9422'                                         
039200*        MOVE ZERO TO MID-BLADDRING-ANT                                   
039300*    END-IF                                                               
039400     SKIP1                                                                
039500                                                                          
039600     IF  IDARTNR-WS NOT NUMERIC                                           
039700         MOVE FEL-1 (SPIND)    TO MOD-TEMFSFEL                            
039800         MOVE NEJ TO SW-NYCKLAR-OK                                        
039900     END-IF.                                                              
040000                                                                          
040100     EJECT                                                                
040200 B-RED-BILD-FRAN-WDK6 SECTION.                                            
040300******************************************************************        
040400*                                                                *        
040500*    REDIGERING AV BILD MED DATA FRÅN WDK6                       *        
040600*                                                                *        
040700******************************************************************        
040800                                                                          
040900*    MOVE +1       TO IX                                                  
041000*    SET MOD-IX    TO 1                                                   
041100*    PERFORM UNTIL IX > MAX-ANT-AO-NUMMER                                 
041200*        MOVE ART-IDAO(IX) TO MOD-IDAO(MOD-IX)                            
041300*        ADD 1 TO IX                                                      
041400*        SET MOD-IX UP BY 1                                               
041500*    END-PERFORM                                                          
041600     SKIP3                                                                
041700     PERFORM IMS-GET-CLAG-SEG                                             
041800     IF  SEGMENT-FINNS                                                    
041900*        MOVE CLAG-IDBERED TO MOD-IDBERED                                 
042000*        MOVE CLAG-IDKAT(1) TO MOD-IDKAT-1                                
042100*        MOVE CLAG-IDKAT(2) TO MOD-IDKAT-2                                
042200*        MOVE CLAG-IDKAT(3) TO MOD-IDKAT-3                                
042300         MOVE CLAG-KDSEGKEY TO W-KDSEGKEY                                 
042400     END-IF                                                               
042500                                                                          
042600*    MOVE 1 TO IX                                                         
042700*    PERFORM UNTIL (IX > MAX-ANT-PROENH)                                  
042800*        MOVE   CLAG-IDPROENH (IX) TO MOD-IDPROENH (IX)                   
042900*        INSPECT MOD-IDPROENH (IX) REPLACING LEADING ZERO BY SPACE        
043000*        ADD 1 TO IX                                                      
043100*    END-PERFORM                                                          
043200     SKIP3                                                                
043300         PERFORM BA-RED-FRAN-MATINFO-WDK611                               
043400     SKIP3                                                                
043500*        MOVE SPACE         TO MOD-FLFSP (1)                              
043600     SKIP3                                                                
043700* ÄT880919 ****************************************************           
043800*                                                                         
043900     MOVE 1                   TO W-KDNOTTYP                               
044000     PERFORM IMS-GET-NOT-SEG                                              
044100                                                                          
044200     IF SEGMENT-FINNS                                                     
044300        MOVE NOT-TEARTNOT     TO MOD-TEARTNOT (1)                         
044400     ELSE                                                                 
044500        MOVE MFS-RENSA-FAELT  TO MOD-TEARTNOT (1)                         
044600     END-IF                                                               
044700                                                                          
044800     MOVE 2                   TO W-KDNOTTYP                               
044900     PERFORM IMS-GET-NOT-SEG                                              
045000                                                                          
045100     IF SEGMENT-FINNS                                                     
045200        MOVE NOT-TEARTNOT     TO MOD-TEARTNOT (2)                         
045300     ELSE                                                                 
045400        MOVE MFS-RENSA-FAELT  TO MOD-TEARTNOT (2)                         
045500     END-IF                                                               
045600                                                                          
045700     MOVE 6                   TO W-KDNOTTYP                               
045800     PERFORM IMS-GET-NOT-SEG                                              
045900                                                                          
046000*    IF SEGMENT-FINNS                                                     
046100*       MOVE NOT-TEARTNOT     TO MOD-VARNOT                               
046200*    ELSE                                                                 
046300*       MOVE MFS-RENSA-FAELT  TO MOD-VARNOT                               
046400*    END-IF                                                               
046500                                                                          
046600*-----------------------------------------------------------------        
046700     SKIP3                                                                
046800*  BORTTAG AV KTO INFÖR SAP 980901  GS  **                                
046900*        MOVE CLAG-IDLKTO      TO W-IDLKTO-N                              
047000*        MOVE W-IDLKTO-3-5     TO W-IDLKTO-3-5-FLYTT                      
047100*        MOVE W-IDLKTO-6-7     TO W-IDLKTO-6-7-FLYTT                      
047200*        MOVE STRECK           TO FILLER-STRECK                           
047300*        MOVE W-IDLKTO-FLYTT-N TO MOD-IDLKTO-3-7                          
047400********************************************                              
047500     SKIP3                                                                
047600         PERFORM BB-RED-FRAN-GEMINFO-WDK611                               
047700     .                                                                    
047800     EJECT                                                                
047900 BA-RED-FRAN-MATINFO-WDK611 SECTION.                                      
048000     SKIP3                                                                
048100*    MOVE CLAG-FLJIT           TO MOD-FLJIT                               
048200*    MOVE CLAG-IDANSK          TO MOD-IDANSK                              
048300*    MOVE CLAG-IDINK           TO MOD-IDINK                               
048400*    MOVE CLAG-IDPLANGR-AG     TO MOD-IDPLANGR-AG                         
048500*    MOVE CLAG-IDPLANGR-LEV TO MOD-IDPLANGR-LEV                           
048600     SKIP1                                                                
048700*    MOVE CLAG-KDAVT          TO MOD-KDAVT                                
048800*    MOVE CLAG-KDHF           TO MOD-KDHF                                 
048900*    MOVE CLAG-KDKSP          TO MOD-KDKSP                                
049000*    MOVE CLAG-KDVVKL         TO MOD-KDVVKL                               
049100*    MOVE CLAG-IDPROJ         TO MOD-IDPROJ                               
049200*    SKIP1                                                                
049300*    MOVE ZERO                TO MOD-KVAL                                 
049400*    MOVE CLAG-KVAP           TO MOD-KVAP                                 
049500*    MOVE CLAG-FLCDART        TO MOD-FLCDART                              
049600*    MOVE CLAG-KVBK           TO MOD-KVBK                                 
049700*    MOVE CLAG-KVDAGAR-INLEV TO MOD-KVDAGAR-INLEV                         
049800*    MOVE CLAG-KVDAGAR-TT     TO MOD-KVDAGAR-TT                           
049900*    MOVE CLAG-KVKP           TO MOD-KVKP                                 
050000*    MOVE CLAG-KVOVERF        TO MOD-KVOVERF                              
050100*    MOVE CLAG-KVPALL         TO MOD-KVPALL                               
050200*    MOVE CLAG-KVQ            TO MOD-KVQ                                  
050300*    MOVE CLAG-KVQ-JUST       TO MOD-KVQ-JUST                             
050400*    MOVE CLAG-TIQJUST        TO MOD-TIQJUST                              
050500*    MOVE CLAG-KVVECKOR-AT TO MOD-KVVECKOR-AT                             
050600*    MOVE CLAG-KVVECKOR-BT TO MOD-KVVECKOR-BT                             
050700*    MOVE CLAG-KVVECKOR-FT TO MOD-KVVECKOR-FT                             
050800*    MOVE CLAG-KVVECKOR-LT TO MOD-KVVECKOR-LT                             
051000     SKIP1                                                                
051100*    MOVE  CLAG-FLAVRART TO MOD-FLAVRART                                  
051200*    MOVE  CLAG-FLMANAT TO MOD-FLMANAT                                    
051300*    MOVE  CLAG-FLMANBK TO MOD-FLMANBK                                    
051400*    MOVE  CLAG-FLMANKP     TO MOD-FLMANKP                                
051500*    MOVE  CLAG-FLMANLT     TO MOD-FLMANLT                                
051600*    MOVE  CLAG-FLMANQ TO MOD-FLMANQ                                      
051700     IF CLAG-FLGEMART = JA                                                
051800        MOVE JA TO WS-FLGEMART                                            
051900        MOVE MED-2 (SPIND) TO MOD-TEMFSINF                                
052000     END-IF                                                               
052100     .                                                                    
052200     EJECT                                                                
052300 BB-RED-FRAN-GEMINFO-WDK611 SECTION.                                      
052400     SKIP3                                                                
052500     SKIP1                                                                
052600*    MOVE CLAG-KDGK TO MOD-KDGK                                           
052700*    MOVE CLAG-KDLTK TO MOD-KDLTK                                         
052800     SKIP1                                                                
052900     MOVE CLAG-KDLEVSP TO W-KDLEVSP-N                                     
053000*    MOVE NEJ TO MOD-KDLEVSP-C1                                           
053100*    MOVE SPACE TO MOD-KDLEVSP-C2                                         
053200*    IF  CLAG-KDLEVSP = 20                                                
053300*        MOVE JA TO MOD-KDLEVSP-C1                                        
053400*    ELSE                                                                 
053500*        IF  W-KDLEVSP-2 = 1                                              
053600*            MOVE JA TO MOD-KDLEVSP-C1                                    
053700*        END-IF                                                           
053800*    END-IF                                                               
053900     SKIP1                                                                
054000*    MOVE  CLAG-FLMANGK       TO MOD-FLMANGK                              
054100*    MOVE  CLAG-KDUART        TO MOD-KDUART                               
054200*    MOVE  CLAG-FLLSRDEL      TO MOD-FLLSRDEL                             
054300*    MOVE  CLAG-FLLTKSP       TO MOD-FLLTKSP                              
054400*    MOVE  CLAG-KDARTURS      TO MOD-KDARTURS                             
054500*    MOVE  CLAG-KDFARLIG      TO MOD-KDFARLIG                             
054600*    MOVE  CLAG-KDSRA         TO MOD-KDSRA                                
054700     .                                                                    
054800     EJECT                                                                
054900 D-RED-BILD-FRAN-WDD3 SECTION.                                            
055000******************************************************************        
055100*                                                                *        
055200*    REDIGERING AV BILD MED DATA FRÅN WDD3                       *        
055300*                                                                *        
055400******************************************************************        
055500*ÄT851015                                                                 
055600     SKIP1                                                                
055700     PERFORM IMS-GET-BENA01-CSEQ                                          
055800     IF MSGI-IDLAND-SPR = 'GB'                                            
055900        MOVE 'GB '          TO W-IDSKYLT                                  
056000     ELSE                                                                 
056100        MOVE 'S  '          TO W-IDSKYLT                                  
056200     END-IF                                                               
056300     PERFORM IMS-GET-BENA11-CSEQ                                          
056400*    MOVE BENA11-TEXT-BEART TO MOD-BEART-SVE.                             
056410     .                                                                    
056500     EJECT                                                                
056600 E-RED-BILD-FRAN-WDN6 SECTION.                                            
056700******************************************************************        
056800*                                                                *        
056900*    REDIGERING AV BILD MED DATA FRÅN WDN6                       *        
057000*                                                                *        
057100******************************************************************        
057200*ÄT861217                                                                 
057300     SKIP1                                                                
057400     PERFORM IMS-GET-MASTER-WDN6                                          
057500     IF SEGMENT-FINNS                                                     
057600        PERFORM IMS-GET-KATINFO-MASTER                                    
057700*       IF MID-BLADDRING-ANT = ZERO                                       
057800*          CONTINUE                                                       
057900*       ELSE                                                              
058000*          SET MOD-IZ    TO +1                                            
058100*          MOVE MID-BLADDRING-ANT TO LASTA-SEG                            
058200*          PERFORM UNTIL MOD-IZ > LASTA-SEG OR SEGMENT-SAKNAS             
058300*             PERFORM IMS-GET-KATINFO-MASTER                              
058400*             SET MOD-IZ  UP BY +1                                        
058500*          END-PERFORM                                                    
058600*       END-IF                                                            
058700*       SET MOD-IZ     TO +1                                              
058800*       PERFORM UNTIL MOD-IZ > 11 OR SEGMENT-SAKNAS                       
058900*          MOVE KATN-KAT-BEEMBLEM TO MOD-IDKAT (MOD-IZ)                   
059000*          ADD +1        TO LASTA-SEG                                     
059100*          PERFORM IMS-GET-KATINFO-MASTER                                 
059200*          SET MOD-IZ UP BY +1                                            
059300*       END-PERFORM                                                       
059400        IF SEGMENT-FINNS                                                  
059500*          MOVE LASTA-SEG  TO MOD-BLADDRING-ANT                           
059600           IF WS-FLGEMART = JA                                            
059700              MOVE MED-3 (SPIND) TO MOD-TEMFSINF                          
059800           ELSE                                                           
059900              MOVE MED-1 (SPIND) TO MOD-TEMFSINF                          
060000           END-IF                                                         
060100        ELSE                                                              
060200*          MOVE ZERO       TO MOD-BLADDRING-ANT                           
060210           CONTINUE                                                       
060300        END-IF                                                            
060400     END-IF.                                                              
060500                                                                          
060600* IMS SEKTIONER                                                           
060700     SKIP3                                                                
060800 IMS-GET-MSG SECTION.                                                     
060900     MOVE '  QC' TO GODK-STATUSKODER                                      
061000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061200     PERFORM IMS-STATUSKONTROLL.                                          
061300     SKIP3                                                                
061400 IMS-INSERT-MSG SECTION.                                                  
061500*    IF MSGI-IDLAND-SPR = 'GB'                                            
061600*       MOVE 'N' TO MFS-KDHUVOMR                                          
061700*    END-IF                                                               
061800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
061900     MOVE SPACE TO GODK-STATUSKODER                                       
062000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
062100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062200     PERFORM IMS-STATUSKONTROLL.                                          
062300     EJECT                                                                
062400 IMS-GET-ART-SEG SECTION.                                                 
062500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
062600            DELIMITED BY SIZE INTO SSA1                                   
062700     MOVE '  GE' TO GODK-STATUSKODER                                      
062800     CALL CBLTDLI USING GU AB-PCB DLI-IO-AREA-01 SSA1                     
062900     MOVE AB-STATUS-CODE TO STATUS-WS                                     
063000     PERFORM IMS-STATUSKONTROLL.                                          
063100     SKIP3                                                                
063200 IMS-GET-CLAG-SEG SECTION.                                                
063300     MOVE 'WLARTC11' TO SSA1                                              
063400     MOVE '  GE' TO GODK-STATUSKODER                                      
063500     CALL CBLTDLI USING GNP AB-PCB DLI-IO-AREA-11 SSA1                    
063600     MOVE AB-STATUS-CODE TO STATUS-WS                                     
063700     PERFORM IMS-STATUSKONTROLL.                                          
063800     SKIP3                                                                
063900 IMS-GET-NOT-SEG SECTION.                                                 
064000     MOVE 'WLARTC11*F' TO SSA1                                            
064100     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
064200            DELIMITED BY SIZE INTO SSA2                                   
064300     MOVE '  GE' TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING GNP AB-PCB DLI-IO-AREA SSA1 SSA2                  
064500     MOVE AB-STATUS-CODE TO STATUS-WS                                     
064600     PERFORM IMS-STATUSKONTROLL.                                          
064700     EJECT                                                                
064800 IMS-GET-BENA01-CSEQ SECTION.                                             
064900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
065000             DELIMITED BY SIZE INTO SSA1                                  
065100     MOVE '  ' TO GODK-STATUSKODER                                        
065200     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA SSA1                      
065300     MOVE BENC-STATUS-CODE  TO STATUS-WS                                  
065400     PERFORM IMS-STATUSKONTROLL.                                          
065500     SKIP3                                                                
065600 IMS-GET-BENA11-CSEQ SECTION.                                             
065700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
065800             DELIMITED BY SIZE INTO SSA1                                  
065900     MOVE '  ' TO GODK-STATUSKODER                                        
066000     CALL CBLTDLI USING GNP BENC-PCB DLI-IO-AREA SSA1                     
066100     MOVE BENC-STATUS-CODE  TO STATUS-WS                                  
066200     PERFORM IMS-STATUSKONTROLL.                                          
066300     EJECT                                                                
066400 IMS-GET-MASTER-WDN6 SECTION.                                             
066500     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
066600             DELIMITED BY SIZE INTO SSA1                                  
066700     MOVE '  GE' TO GODK-STATUSKODER                                      
066800     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA SSA1                      
066900     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
067000     PERFORM IMS-STATUSKONTROLL.                                          
067100     SKIP3                                                                
067200 IMS-GET-KATINFO-MASTER SECTION.                                          
067300     MOVE 'WLKATN11 ' TO SSA1                                             
067400     MOVE '  GE' TO GODK-STATUSKODER                                      
067500     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA SSA1                     
067600     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
067700     PERFORM IMS-STATUSKONTROLL.                                          
067800     SKIP3                                                                
067900 IMS-STATUSKONTROLL SECTION.                                              
068000     SET STATUS-IX TO 1                                                   
068100     SEARCH GODK-STATUS AT END CALL FELLOG                                
068200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
068300     END-SEARCH.                                                          
