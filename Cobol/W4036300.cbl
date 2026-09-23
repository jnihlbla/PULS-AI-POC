000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036300.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   90/03/07.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        UNDERHÅLL AV PRODUKTIONSTIDSMATRIS.                              
001100*                                                                         
001200*        PROGRAMMET VISAR OCH GÖR FÖRÄNDRINGAR MOT DEN TABELL SOM         
001300*        VISAR HUR DEN PRODUKTIONSTIDSMATRIS SER UT SOM AVGÖR             
001400*        VILKEN TOTAL PRODUKTIONSTID ORDERN FÅR.                          
001500*        DENNA TID ANVÄNDS TILL ATT, FRÅN READY FOR SHIPMENT,             
001600*        RÄKNA UT NÄR ORDERN SENAST MÅSTE PÅBÖRJAS FÖR ATT HINNA          
001700*        MED TRANSPORTEN.                                                 
001800*                                                                         
001900*        PROGRAMMET INGÅR I WOPS-SYSTEMET I PD-90.                        
002000*                                                                         
002100*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002200*        PROGRAMMET UPPDATERAR WLXXKI (WDR1)                              
002300*                                                                         
002400*                                                                         
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W4T363                                              
002800*        MID:         W4I36301                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W4O36301                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4036300'.            
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  MOD-INDX                    PIC S9(3)  VALUE +0    COMP-3.           
004400 77  MAX-X-IX                    PIC S9(3)  VALUE +0    COMP-3.           
004500 77  4452-RAD-IX                 PIC S9(3)  VALUE +0    COMP-3.           
004700 77  HELP-INDX                   PIC S9(3)  VALUE +0    COMP-3.           
004800 77  L-INDX                      PIC S9(3)  VALUE +0    COMP-3.           
004900 77  4452-KVPTSORT-X-IX          PIC S9(3)  VALUE +0    COMP-3.           
005000 77  X-INDX                      PIC S9(3)  VALUE +0    COMP-3.           
005100 77  Y-INDX                      PIC S9(3)  VALUE +0    COMP-3.           
005110*Z = Z-AXEL????                                                           
005200 77  Z-INDX                      PIC S9(3)  VALUE +0    COMP-3.           
005300 77  SPAR-INDX                   PIC S9(3)  VALUE +0    COMP-3.           
005400 77  MAX-VARDE                   PIC S9(4)V9 VALUE 9999.9 COMP-3.         
005500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005600 77  MAX-MOD-LAENGD              PIC S9(5)  VALUE +932  COMP SYNC.        
005700 77  KVPTSORT-IN-NUM             PIC S9(4)V9(1) VALUE +0 COMP-3.          
005800 77  KVPTSORT-TID                PIC S9(4)V9(1) VALUE +0 COMP-3.          
005900                                                                          
006000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006100 77  WS-IDPTIDTAB                PIC X(2)    VALUE SPACE.                 
006401     EJECT                                                                
006410                                                                          
006420 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006430     88  INDATA-OK                           VALUE 'J'.                   
006500     88  INDATA-FEL                          VALUE 'N'.                   
006600                                                                          
006700 77  INPUT-SW                    PIC X       VALUE 'J'.                   
006800     88  NO-INPUT                            VALUE 'J'.                   
006900                                                                          
007000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007100     88  ALLT-OK                             VALUE 'J'.                   
007200                                                                          
007300 77  SVAR-SW                     PIC X       VALUE 'N'.                   
007400     88  GODK-SVAR                           VALUE 'J' 'Y'.               
007500                                                                          
007600 77  AENDRING-SW                 PIC X       VALUE 'N'.                   
007700     88  AENDRING                            VALUE 'J'.                   
007800                                                                          
007900 77  FIRST-TIME-SW               PIC X       VALUE 'N'.                   
008000     88  FIRST-TIME                          VALUE 'J'.                   
008100                                                                          
008200 77  TABELL-SW                   PIC X       VALUE 'N'.                   
008300     88  NY-TABELL                           VALUE 'J'.                   
008400                                                                          
008500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008600     88  NYCKLAR-OK                          VALUE 'J'.                   
008700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008800                                                                          
008900 77  SORT-KOD                    PIC X(2)    VALUE SPACE.                 
009000     88  GODK-SORTKOD                        VALUE 'M3' 'KG'              
009100                                                   'OL' 'MS'              
009200                                                   'ST'.                  
009300                                                                          
009400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009500     88  EGEN-MID                            VALUE '4363'.                
009600     88  GODK-MID                            VALUE '4361' '4362'          
009700                                                   '4363' '4366'.         
009800                                                                          
009900 77  BASTAB                      PIC X(2)    VALUE SPACE.                 
010000     88  BASTABELL                           VALUE ' 1' '01'.             
010100                                                                          
010200 01  MID-HELP-AREA-NUM.                                                   
010300     03  HELP-RAD-NUM            OCCURS 10 TIMES                          
010400                                 INDEXED BY MID-NUM-IX.                   
010500         05   HELP-MID-AREA      PIC S9(4)V9(1) COMP-3.                   
010600                                                                          
010700     EJECT                                                                
010800 01  TABELL.                                                              
010900     03  HELP-RAD                OCCURS 10 TIMES                          
011000                                 INDEXED BY HELP-Y-IX.                    
011100         05   HELP-KVPTSORT-Y    PIC 9(4)V9(1).                           
011200         05   HELP-KVPTSORT-X    PIC 9(4)V9(1).                           
011300         05   HELP-KOL           OCCURS 10 TIMES                          
011400                                 INDEXED BY HELP-X-IX.                    
011500              07   HELP-KVPTID   PIC 9(2)V9.                              
011600                                                                          
011700                                                                          
011800 01  SPARAREA-4451.                                                       
011900     03  SPAR-IDHTYP             PIC X(4).                                
012000     03  SPAR-IDDC               PIC X(2).                                
012100     03  SPAR-IDPTIDTAB          PIC 9(2).                                
012200     03  SPAR-LOW-VALUE          PIC X(22).                               
012300                                                                          
012400 01  SPARAREA-4452.                                                       
012500     03  FILLER                  PIC X(320).                              
012600                                                                          
012700     EJECT                                                                
012800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012900 01  GENERELLA-SUBPROGRAM.                                                
013000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013310     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013400     EJECT                                                                
013500*                                                                         
013600*    --- PARAMETRAR TILL SUBPROGRAM WDECAREA                              
013700*   -COPY WDECAREA                                                        
013900     SKIP2                                                                
014000*                                                                         
014100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014200*   -COPY WMEDAREA                                                        
014300     EJECT                                                                
014310*                   ****    PARAMETRAR TILL W005INIT                      
014320*01  -COPY WMSGINIT                                                       
014400     SKIP3                                                                
014500 01  MESSAGE-CODES.                                                       
014600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015100     EJECT                                                                
015200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015300*                                                                         
015400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015500     SKIP3                                                                
015600*01  MID -COPY W4I36301     -PRE MID-                                     
015800     EJECT                                                                
015900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016000     SKIP3                                                                
016100*01  -COPY WMSGAREA                                                       
016300     EJECT                                                                
016400*    03  MOD -COPY W4O36301   -RED MSG-AREA  -PRE MOD-                    
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016800     SKIP3                                                                
016900*01  -COPY WMFSAREA                                                       
017100     EJECT                                                                
017200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017500     SKIP3                                                                
017600 01  NYCKLAR-TILL-DLI.                                                    
017700     03  W-IDPTIDTAB-X.                                                   
017800         05  W-IDHTYP            PIC X(4)     VALUE '4451'.               
017900         05  W-IDDC              PIC X(2).                                
018000         05  W-IDPTIDTAB         PIC X(2)     VALUE SPACE.                
018100         05  W-LOW-VALUE         PIC X(22)    VALUE LOW-VALUE.            
018200                                                                          
018300     03  W-KDSEGKEY-X.                                                    
018400         05  W-KDSEGKEY          PIC X(01)    VALUE '1'.                  
018500                                                                          
018510     03  W-IDDC-B6-X.                                                     
018520         05 W-IDDC-B6                  PIC X(2).                          
018530                                                                          
018600*    --- STATUS-KOD FRÅN IMS                                              
018700 01  STATUS-WS                   PIC XX.                                  
018800     88  SEGMENT-FINNS                       VALUE '  '.                  
018900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019100     SKIP2                                                                
019200 01  GODK-STATUSKODER.                                                    
019300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019400     SKIP3                                                                
019500 01  SSA1                        PIC X(64).                               
019600 01  SSA2                        PIC X(64).                               
019700     EJECT                                                                
019800*    --- IMS FUNKTIONSKODER                                               
019900*01  -COPY W0003                                                          
020100     EJECT                                                                
020200*    ---  DLI INPUT-OUTPUT AREA                                           
020300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020400     SKIP3                                                                
020500 01  DLI-IO-AREA.                                                         
020600     03  IO-AREA                 PIC X(320)  VALUE SPACE.                 
020700     SKIP3                                                                
020800     03  WLXXKI01 REDEFINES IO-AREA.                                      
020900*        05  -COPY WDGX4451   -PRE XXKI-                                  
021100     EJECT                                                                
021200     03  WLXXKI11 REDEFINES IO-AREA.                                      
021300*        05  -COPY WDGX4452   -PRE XXKI-                                  
021400                                                                          
021410 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
021420 01   DLI-IO-AREA-B601.                                                   
021430*     03  -COPY WDB601                                                    
021440                                                                          
021500     EJECT                                                                
021600 LINKAGE SECTION.                                                         
021700                                                                          
021800*01  -COPY W0009      -PRE MSG-                                           
021900     EJECT                                                                
021910*01  -COPY W0008     -PRE USEA-                                           
021920     05  FILLER              PIC X.                                       
022000     EJECT                                                                
022100*01  -COPY W0008      -PRE XXKI-                                          
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022410*01  -COPY W0008      -PRE WDB6-                                          
022420     05  FILLER                  PIC X.                                   
022430     EJECT                                                                
022500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
022510                                   XXKI-PCB WDB6-PCB.                     
022600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
022610                                   XXKI-PCB WDB6-PCB.                     
022700                                                                          
022800     PERFORM IMS-GET-MSG                                                  
022900     IF SEGMENT-FINNS                                                     
023000       PERFORM A-INIT                                                     
023100       PERFORM B-KOLLA-NYCKLAR                                            
023200       IF NYCKLAR-OK                                                      
023300         IF MFS-UPDATE                                                    
023400           PERFORM G-KOLLA-INPUT                                          
023500           IF INDATA-OK                                                   
023600             PERFORM H-UPPDATERA                                          
023700           END-IF                                                         
023800         ELSE                                                             
023900           PERFORM MFS-RENSA-FAELT-IN                                     
024000           PERFORM S01-LAES-VISA-INFO                                     
024100           MOVE NEJ TO MOD-JA-NEJ-SW                                      
024200         END-IF                                                           
024300       END-IF                                                             
024400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
024500       PERFORM IMS-INSERT-MSG                                             
024600     END-IF                                                               
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     IF MSG-DUBBLA-TRANSKODER                                             
025500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36301                 
025600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
025700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025800     ELSE                                                                 
025900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I36301                  
026000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026200     END-IF                                                               
026300                                                                          
026400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
026500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
026600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026700                                                                          
026800     MOVE LOW-VALUE TO MSG-AREA                                           
026900     MOVE 'W4O363N1' TO MFS-IDMOD                                         
027000     MOVE '4363' TO MOD-IDTRANS                                           
027100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027200                                                                          
027300     IF NOT EGEN-MID                                                      
027400       MOVE SPACE TO MFS-KDTRTYP                                          
027500       MOVE '7' TO MFS-IDPFK                                              
027600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 B-KOLLA-NYCKLAR SECTION.                                                 
028910                                                                          
028920     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028930     MOVE '001'             TO MSGI-KDCALL                                
028940     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
028941     MOVE '4363'            TO MSGI-IDTRANS                               
028942     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
028950     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
028951                                                                          
028952     IF MSGI-IDLAND-SPR = 'GB'                                            
028953       MOVE +2 TO SPRAK-IX                                                
028954       MOVE 'GB ' TO MED-IDSKYLT                                          
028955     ELSE                                                                 
028956       MOVE +1 TO SPRAK-IX                                                
028957       MOVE 'S  ' TO MED-IDSKYLT                                          
028958     END-IF                                                               
028960                                                                          
029000     IF GODK-MID                                                          
029100       MOVE LOW-VALUE TO XXKI-4451-LOW-VALUE                              
029200       MOVE JA TO NYCKLAR-SW                                              
029300                                                                          
029400*      -- KONTROLL AV IDPTIDTAB                                           
029500       MOVE MFS-RENSA-FAELT TO MOD-IDPTIDTAB-IN                           
029600       MOVE MFS-RENSA-FAELT TO MOD-COPY-TAB                               
029700                                                                          
029800       IF MID-IDPTIDTAB-IN = ALL '+'                                      
029900         MOVE MID-IDPTIDTAB-UT TO BASTAB                                  
030000         INSPECT BASTAB REPLACING LEADING SPACE BY ZERO                   
030100       ELSE                                                               
030200         MOVE MID-IDPTIDTAB-IN TO BASTAB                                  
030300         INSPECT BASTAB REPLACING LEADING SPACE BY ZERO                   
030400       END-IF                                                             
030410                                                                          
030420       MOVE MSGI-IDDC               TO W-IDDC-B6                          
030430       PERFORM IMS-GU-WDB601                                              
030584                                                                          
030585       IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                       
030586         MOVE NEJ                     TO NYCKLAR-SW                       
030591       END-IF                                                             
030592                                                                          
030593       IF NYCKLAR-OK                                                      
030594         MOVE DCS-IDDC         TO W-IDDC                                  
030595       END-IF                                                             
030596                                                                          
030600       IF BASTAB = ZERO                                                   
030700         MOVE NEJ TO NYCKLAR-SW                                           
030800       END-IF                                                             
030900                                                                          
031000       IF BASTAB NOT NUMERIC                                              
031100         MOVE NEJ TO NYCKLAR-SW                                           
031200         MOVE '001' TO MED-IDMFSFEL                                       
031300*****    TABELLID EJ NUMERISKT **************                             
031400         CALL WMEDKONV USING MED-WMEDAREA                                 
031500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
031600         PERFORM MFS-RENSA-FAELT-IN                                       
031700         PERFORM MFS-RENSA-FAELT-UT                                       
031800         MOVE BASTAB TO MOD-IDPTIDTAB-UT                                  
031900       ELSE                                                               
032000                                                                          
032100         IF BASTABELL                                                     
032200           IF MFS-UPDATE  AND MID-IDPTIDTAB-IN = ALL '+'                  
032300             MOVE 'M' TO NYCKLAR-SW                                       
032400             MOVE '777' TO MED-IDMFSINF                                   
032500*****        UPPDATERING EJ TILLÅTEN **************                       
032600             CALL WMEDKONV USING MED-WMEDAREA                             
032700             MOVE MED-MFSINF TO MOD-TEMFSINF                              
032800             PERFORM MFS-ROER-EJ-FAELT-UT                                 
032900             PERFORM MFS-RENSA-FAELT-IN                                   
033000             MOVE MID-IDPTIDTAB-UT TO MOD-IDPTIDTAB-UT                    
033100           END-IF                                                         
033200         END-IF                                                           
033300                                                                          
033400         IF NYCKLAR-OK                                                    
033500           IF MID-IDPTIDTAB-IN = ALL '+'                                  
033600             MOVE MID-IDPTIDTAB-UT TO WS-IDPTIDTAB                        
033700             INSPECT WS-IDPTIDTAB REPLACING LEADING SPACE BY ZERO         
033800           ELSE                                                           
033900             MOVE MID-IDPTIDTAB-IN TO WS-IDPTIDTAB                        
034000             INSPECT WS-IDPTIDTAB REPLACING LEADING SPACE BY ZERO         
034100             MOVE ' '   TO MFS-IDPFK                                      
034200             MOVE SPACE TO MFS-KDTRTYP                                    
034300           END-IF                                                         
034400         END-IF                                                           
034500                                                                          
034600         MOVE WS-IDPTIDTAB TO W-IDPTIDTAB                                 
034700                                                                          
034800         IF NYCKLAR-FEL                                                   
034900           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
035000           CALL WMEDKONV USING MED-WMEDAREA                               
035100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
035200           PERFORM MFS-RENSA-FAELT-IN                                     
035300           PERFORM MFS-RENSA-FAELT-UT                                     
035400         END-IF                                                           
035500       END-IF                                                             
035600                                                                          
035700       IF GODK-MID OR NYCKLAR-OK                                          
035800         MOVE BASTAB     TO MOD-IDPTIDTAB-UT                              
035900         INSPECT MOD-IDPTIDTAB-UT REPLACING LEADING ZERO BY SPACE         
036000       ELSE                                                               
036100         MOVE BASTAB TO MOD-IDPTIDTAB-UT                                  
036200       END-IF                                                             
036300       IF MFS-QUERY  AND EGEN-MID AND NYCKLAR-SW NOT = 'M'                
036400         MOVE +1 TO 4452-RAD-IX                                           
036500         PERFORM 10 TIMES                                                 
036600           IF MID-KVPTSORT(4452-RAD-IX) NOT = ALL '+'                     
036700             MOVE NEJ TO INPUT-SW                                         
036800             MOVE MFS-ADD-LAES-IN-FAELT TO                                
036900                          MOD-KVPTSORT-ATTR(4452-RAD-IX)                  
037000             MOVE MFS-ROER-EJ-FAELT   TO                                  
037100                          MOD-KVPTSORT(4452-RAD-IX)                       
037200           END-IF                                                         
037300           ADD +1 TO 4452-RAD-IX                                          
037400         END-PERFORM                                                      
037500         IF INPUT-SW = NEJ                                                
037600           IF MFS-QUERY                                                   
037700             MOVE INF-PRESS-PF11 TO MED-IDMFSINF                          
037800             CALL WMEDKONV USING MED-WMEDAREA                             
037900             MOVE MED-MFSINF TO MOD-TEMFSINF                              
038000             PERFORM MFS-ROER-EJ-FAELT-UT                                 
038100             MOVE MFS-ADD-LAES-IN-FAELT TO                                
038200                          MOD-KVPTSORT-IN-ATTR                            
038300                          MOD-JA-NEJ-SW-ATTR                              
038400             MOVE MFS-ROER-EJ-FAELT   TO                                  
038500                          MOD-KVPTSORT-IN                                 
038600                          MOD-JA-NEJ-SW                                   
038700             MOVE NEJ TO NYCKLAR-SW                                       
038800           END-IF                                                         
038900         END-IF                                                           
039000       END-IF                                                             
039100       IF NYCKLAR-SW = 'M'                                                
039200         MOVE NEJ TO NYCKLAR-SW                                           
039300       END-IF                                                             
039400     ELSE                                                                 
039500       MOVE MFS-RENSA-FAELT TO MOD-IDPTIDTAB-IN                           
039600                               MOD-IDPTIDTAB-UT                           
039700       PERFORM MFS-RENSA-FAELT-IN                                         
039800       PERFORM MFS-RENSA-FAELT-UT                                         
039900       MOVE NEJ TO NYCKLAR-SW                                             
040000     END-IF                                                               
040010     MOVE DCS-IDDC          TO MOD-IDDC-UT                                
040100     .                                                                    
040200     EJECT                                                                
040300                                                                          
040400 G-KOLLA-INPUT SECTION.                                                   
040500                                                                          
040600     MOVE JA TO ALLT-SW                                                   
040700     MOVE MID-JA-NEJ-SW TO SVAR-SW                                        
040800     IF MID-JA-NEJ-SW = 'J' OR                                            
040900        MID-JA-NEJ-SW = 'N'                                               
041000       MOVE NEJ TO MOD-JA-NEJ-SW                                          
041100     END-IF                                                               
041200                                                                          
041300     MOVE JA  TO INDATA-SW                                                
041500     IF MID-IDPTIDTAB-UT NUMERIC AND                                      
041600        MID-COPY-TAB NOT = ALL '+'                                        
041700       IF MID-COPY-TAB NUMERIC                                            
041800         PERFORM GAA-KOPIERA-GAMMAL-TABELL                                
041900         IF ALLT-OK                                                       
042000           PERFORM S01-LAES-VISA-INFO                                     
042100         END-IF                                                           
042200       ELSE                                                               
042300         MOVE '001' TO MED-IDMFSFEL                                       
042400*****    TABELLID EJ NUMERISKT **************                             
042500         CALL WMEDKONV USING MED-WMEDAREA                                 
042600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
042700         MOVE MFS-ALFA-FAELT-FEL TO MOD-COPY-TAB-ATTR                     
042800         MOVE MFS-ROER-EJ-FAELT  TO MOD-COPY-TAB                          
042900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
043000         MOVE NEJ TO INDATA-SW                                            
043100       END-IF                                                             
043200     END-IF                                                               
043400     IF INDATA-OK                                                         
043500       PERFORM IMS-GET-XXKI-WDGX4451                                      
043600       IF SEGMENT-SAKNAS                                                  
043700         PERFORM GA-SKAPA-NY-TABELL                                       
043800         PERFORM S01-LAES-VISA-INFO                                       
043900       ELSE                                                               
044000         IF MID-KVPTSORT-IN = ALL '+'                                     
044100           PERFORM GD-FELMED-413                                          
044200         ELSE                                                             
044300           PERFORM IMS-GET-XXKI-WDGX4452                                  
044400           IF MID-KVPTSORT-IN NOT = ALL '+'                               
044500             PERFORM GE-KOLLA-KVPTSORT-IN                                 
044600           ELSE                                                           
044700             MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-IN-ATTR               
044800             MOVE NEJ TO INDATA-SW                                        
044900           END-IF                                                         
045000                                                                          
045100           MOVE 1 TO X-INDX                                               
045300           PERFORM UNTIL INPUT-SW = NEJ OR X-INDX > 10                    
045400              IF MID-KVPTSORT(X-INDX) NOT = ALL '+'                       
045500                MOVE NEJ TO INPUT-SW                                      
045600              END-IF                                                      
045700              ADD 1 TO X-INDX                                             
045800           END-PERFORM                                                    
045900           IF NO-INPUT AND MID-KDSORT-IN = ALL '+'                        
046000             IF KVPTSORT-IN-NUM = ZERO                                    
046100               PERFORM GF-FELMED-ERR-PF11-AND-NO-DATA                     
046200             END-IF                                                       
046300           ELSE                                                           
046400             IF NO-INPUT AND MID-KDSORT-IN NOT = ALL '+'                  
046500               PERFORM GB-BARA-SORTKOD-IFYLLD                             
046600             ELSE                                                         
046700               IF NOT INDATA-FEL                                          
046800                 PERFORM GC-INDATA-OCH-SORT-IFYLLDA                       
046900               END-IF                                                     
047000             END-IF                                                       
047100           END-IF                                                         
047200           IF SVAR-SW NOT = 'N'                                           
047300             PERFORM GJ-KOLLA-JA-NEJ-SW                                   
047400           END-IF                                                         
047500         END-IF                                                           
047600       END-IF                                                             
047700                                                                          
047800       IF INDATA-FEL                                                      
047900         PERFORM GG-INDATA-FEL                                            
048000       END-IF                                                             
048100                                                                          
048200       IF INDATA-SW = 'K'                                                 
048300         PERFORM GH-INDATA-SW-AER-K                                       
048400       END-IF                                                             
048500                                                                          
048600       IF INDATA-SW = 'L'                                                 
048700         PERFORM GK-INDATA-SW-AER-L                                       
048800       END-IF                                                             
048900                                                                          
049000       IF INDATA-SW = 'F'                                                 
049100         PERFORM GI-INDATA-SW-AER-F                                       
049200       END-IF                                                             
049300                                                                          
049400       IF KVPTSORT-IN-NUM = MAX-VARDE                                     
049500         IF NO-INPUT                                                      
049600           MOVE '007' TO MED-IDMFSINF                                     
049700*******************  OTILLÅTEN UPPDATERING ***********************        
049800           CALL WMEDKONV USING MED-WMEDAREA                               
049900           MOVE MED-MFSINF TO MOD-TEMFSINF                                
050000           MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                           
050100           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-IN-ATTR                 
050200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
050300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
050400           MOVE NEJ TO INDATA-SW                                          
050500         END-IF                                                           
050600       END-IF                                                             
050700                                                                          
050800       IF INDATA-SW = 'W' OR                                              
050900          INDATA-SW = 'K' OR                                              
051000          INDATA-SW = 'L' OR                                              
051100          INDATA-SW = 'F'                                                 
051200         MOVE NEJ TO INDATA-SW                                            
051300       END-IF                                                             
051400     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800 GA-SKAPA-NY-TABELL SECTION.                                              
052000                                                                          
052100     MOVE 01 TO W-IDPTIDTAB                                               
052200     PERFORM IMS-GET-XXKI-WDGX4451                                        
052300     MOVE WS-IDPTIDTAB TO W-IDPTIDTAB XXKI-4451-IDPTIDTAB                 
052400     MOVE XXKI-4451-WDGX4451 TO SPARAREA-4451                             
052500     PERFORM IMS-GET-XXKI-WDGX4452                                        
052600     MOVE XXKI-4452-WDGX4452 TO SPARAREA-4452                             
052700     MOVE SPARAREA-4451 TO XXKI-4451-WDGX4451                             
052800     PERFORM IMS-ISRT-XXKI-WDGX4451                                       
052900     MOVE SPARAREA-4452 TO XXKI-4452-WDGX4452                             
053000     PERFORM IMS-ISRT-XXKI-WDGX4452                                       
053100     MOVE JA  TO TABELL-SW                                                
053200     MOVE NEJ TO INDATA-SW                                                
053300     MOVE '024' TO MED-IDMFSINF                                           
053400*************** GENERELL TABELL UPPLAGD ******************                
053500     CALL WMEDKONV USING MED-WMEDAREA                                     
053600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
053700                                                                          
053800     .                                                                    
053900     EJECT                                                                
054000 GAA-KOPIERA-GAMMAL-TABELL SECTION.                                       
054200                                                                          
054300     PERFORM IMS-GET-XXKI-WDGX4451                                        
054400     IF SEGMENT-FINNS                                                     
054500       MOVE MID-COPY-TAB TO W-IDPTIDTAB                                   
054600       PERFORM IMS-GET-XXKI-WDGX4451                                      
054700       IF SEGMENT-FINNS                                                   
054800         PERFORM IMS-GET-XXKI-WDGX4452                                    
054900         MOVE XXKI-4452-WDGX4452 TO SPARAREA-4452                         
055000         MOVE WS-IDPTIDTAB TO W-IDPTIDTAB                                 
055100         PERFORM IMS-GHU-XXKI-WDGX4451                                    
055200         PERFORM IMS-GET-XXKI-WDGX4452                                    
055300         PERFORM IMS-DLET-XXKI-WDGX4452                                   
055400         PERFORM IMS-GHU-XXKI-WDGX4451                                    
055500         MOVE SPARAREA-4452 TO XXKI-4452-WDGX4452                         
055600         PERFORM IMS-ISRT-XXKI-WDGX4452                                   
055700         MOVE JA TO TABELL-SW                                             
055800         MOVE NEJ TO INDATA-SW                                            
055900         MOVE '101' TO MED-IDMFSINF                                       
056000***************     UPPDATERING UTFÖRD    ******************              
056100         CALL WMEDKONV USING MED-WMEDAREA                                 
056200         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
056300       ELSE                                                               
056400         MOVE '023' TO MED-IDMFSFEL                                       
056500***************     TABELL SAKNAS    ***********************              
056600         CALL WMEDKONV USING MED-WMEDAREA                                 
056700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
056800         MOVE MFS-NUM-FAELT-FEL TO MOD-COPY-TAB-ATTR                      
056900         MOVE MFS-ROER-EJ-FAELT TO MOD-COPY-TAB                           
057000         MOVE WS-IDPTIDTAB TO W-IDPTIDTAB                                 
057100         MOVE NEJ TO INDATA-SW                                            
057200         MOVE JA TO TABELL-SW                                             
057300       END-IF                                                             
057400     ELSE                                                                 
057500       MOVE MID-COPY-TAB TO W-IDPTIDTAB                                   
057600       PERFORM IMS-GET-XXKI-WDGX4451                                      
057700       IF SEGMENT-FINNS                                                   
057800          MOVE W-IDPTIDTAB TO XXKI-4451-IDPTIDTAB                         
057900          MOVE XXKI-4451-WDGX4451 TO SPARAREA-4451                        
058000          PERFORM IMS-GET-XXKI-WDGX4452                                   
058100          MOVE XXKI-4452-WDGX4452 TO SPARAREA-4452                        
058200          MOVE SPARAREA-4451 TO XXKI-4451-WDGX4451                        
058300          PERFORM IMS-ISRT-XXKI-WDGX4451                                  
058400          MOVE SPARAREA-4452 TO XXKI-4452-WDGX4452                        
058500          PERFORM IMS-ISRT-XXKI-WDGX4452                                  
058600         MOVE JA TO TABELL-SW                                             
058700         MOVE NEJ TO INDATA-SW                                            
058800         MOVE '101' TO MED-IDMFSINF                                       
058900***************     UPPDATERING UTFÖRD    ******************              
059000         CALL WMEDKONV USING MED-WMEDAREA                                 
059100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
059200       ELSE                                                               
059300         MOVE '023' TO MED-IDMFSFEL                                       
059400***************     TABELL SAKNAS    ***********************              
059500         CALL WMEDKONV USING MED-WMEDAREA                                 
059600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
059700         MOVE MFS-NUM-FAELT-FEL TO MOD-COPY-TAB-ATTR                      
059800         MOVE MFS-ROER-EJ-FAELT TO MOD-COPY-TAB                           
059900         MOVE WS-IDPTIDTAB TO W-IDPTIDTAB                                 
060000         MOVE NEJ TO INDATA-SW                                            
060100         MOVE JA TO TABELL-SW                                             
060200       END-IF                                                             
060300     END-IF                                                               
060400     .                                                                    
060500     EJECT                                                                
060600 GB-BARA-SORTKOD-IFYLLD SECTION.                                          
060800                                                                          
060900     MOVE MID-KDSORT-IN TO SORT-KOD                                       
061000     IF GODK-SORTKOD                                                      
061100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-IN-ATTR                    
061200     ELSE                                                                 
061300       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR                      
061400       MOVE '013' TO MED-IDMFSINF                                         
061500****************** EJ GODKÄND SORTKOD ****************************        
061600       CALL WMEDKONV USING MED-WMEDAREA                                   
061700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
061800       MOVE NEJ TO INDATA-SW                                              
061900     END-IF                                                               
062000                                                                          
062100     .                                                                    
062200                                                                          
062300     EJECT                                                                
062400 GC-INDATA-OCH-SORT-IFYLLDA SECTION.                                      
062600                                                                          
062700     SET MID-NUM-IX TO +1                                                 
062800     MOVE +1 TO SPAR-INDX                                                 
062900     MOVE +1 TO MAX-X-IX                                                  
063000     IF  GODK-SVAR              AND                                       
063100         MID-KVPTSORT-IN = ZERO AND                                       
063200            XXKI-4452-KVPTSORT-X(10) = MAX-VARDE  AND                     
063300            MID-KVPTSORT(SPAR-INDX) NOT = ZERO                            
063400         MOVE 'F' TO INDATA-SW                                            
063500     ELSE                                                                 
063600     PERFORM 10 TIMES                                                     
063700       IF MID-KVPTSORT(SPAR-INDX) NOT = ALL '+'                           
063800         MOVE MID-KVPTSORT(SPAR-INDX) TO DEC-IDFRIDATA                    
063900         IF KVPTSORT-IN-NUM > ZERO                                        
064000           MOVE +2 TO DEC-KVHELTAL                                        
064100           MOVE +1 TO DEC-KVDECIMAL                                       
064200         ELSE                                                             
064300           MOVE +4 TO DEC-KVHELTAL                                        
064400           MOVE +1 TO DEC-KVDECIMAL                                       
064500         END-IF                                                           
064600         CALL WDECEDIT USING DEC-WDECAREA                                 
064700         IF DEC-KDSVAR-FEL                                                
064800           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-ATTR(SPAR-INDX)         
064900           MOVE NEJ TO INDATA-SW                                          
065000         ELSE                                                             
065100            MOVE MFS-NUM-FAELT-RAETT TO                                   
065200                               MOD-KVPTSORT-ATTR(SPAR-INDX)               
065300            MOVE DEC-IDEDITDATA TO HELP-MID-AREA(MID-NUM-IX)              
065400            SET MID-NUM-IX UP BY 1                                        
065500            ADD +1 TO MAX-X-IX                                            
065600         END-IF                                                           
065700*--JF 900712                                                              
065800       ELSE                                                               
065900         MOVE ZERO TO HELP-MID-AREA(MID-NUM-IX)                           
066000         SET MID-NUM-IX UP BY 1                                           
066100*--JF SLUT                                                                
066200       END-IF                                                             
066300       ADD +1 TO SPAR-INDX                                                
066400     END-PERFORM                                                          
066500*    IF KVPTSORT-IN-NUM > ZERO                                            
066600*      PERFORM S03-KOLLA-STORLEK                                          
066700*      SUBTRACT 1 FROM MAX-X-IX                                           
066800*      IF MAX-X-IX > 4452-RAD-IX                                          
066900*        MOVE 'K' TO INDATA-SW                                            
067000*      END-IF                                                             
067100*      ADD +1 TO MAX-X-IX                                                 
067200*    END-IF                                                               
067300     IF  MID-KVPTSORT-IN > ZERO                                           
067400       AND  XXKI-4452-KVPTSORT-Y(10) = MAX-VARDE                          
067500         PERFORM HD-SAETT-INDEX                                           
067600       IF   XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) =                        
067700            KVPTSORT-IN-NUM                                               
067800         CONTINUE                                                         
067900       ELSE                                                               
068000         MOVE 'F' TO INDATA-SW                                            
068100       END-IF                                                             
068200     END-IF                                                               
068300                                                                          
068400     IF MID-KDSORT-IN NOT = ALL '+'                                       
068500       PERFORM GB-BARA-SORTKOD-IFYLLD                                     
068600     END-IF                                                               
068700     END-IF                                                               
068800                                                                          
068900     .                                                                    
069000                                                                          
069100     EJECT                                                                
069200                                                                          
069300 GD-FELMED-413 SECTION.                                                   
069500                                                                          
069600     MOVE 'W' TO INDATA-SW                                                
069700     MOVE '413' TO MED-IDMFSFEL                                           
069800************** INFORMATION SAKNAS ****************************            
069900     CALL WMEDKONV USING MED-WMEDAREA                                     
070000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
070100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
070200     PERFORM MFS-ROER-EJ-FAELT-UT                                         
070300     MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-IN-ATTR                       
070400     MOVE +1 TO 4452-RAD-IX                                               
070500     PERFORM 10 TIMES                                                     
070600       IF MID-KVPTSORT(4452-RAD-IX) NOT = ALL '+'                         
070700         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
070800                      MOD-KVPTSORT-ATTR(4452-RAD-IX)                      
070900         MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT(4452-RAD-IX)              
071000       END-IF                                                             
071100     ADD +1 TO 4452-RAD-IX                                                
071200     END-PERFORM                                                          
071300     .                                                                    
071400                                                                          
071500     EJECT                                                                
071600                                                                          
071700 GE-KOLLA-KVPTSORT-IN SECTION.                                            
071800                                                                          
071900     MOVE MID-KVPTSORT-IN TO DEC-IDFRIDATA                                
072000     MOVE +4 TO DEC-KVHELTAL                                              
072100     MOVE +1 TO DEC-KVDECIMAL                                             
072200     CALL WDECEDIT USING DEC-WDECAREA                                     
072300     IF DEC-KDSVAR-FEL                                                    
072400       MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-IN-ATTR                     
072500       MOVE NEJ TO INDATA-SW                                              
072600     ELSE                                                                 
072700       MOVE DEC-IDEDITDATA TO KVPTSORT-IN-NUM                             
072800       IF KVPTSORT-IN-NUM <= MAX-VARDE                                    
072900         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPTSORT-IN-ATTR                 
073000       ELSE                                                               
073100         MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-IN-ATTR                   
073200         MOVE NEJ TO INDATA-SW                                            
073300       END-IF                                                             
073400     END-IF                                                               
073500     .                                                                    
073600                                                                          
073700     EJECT                                                                
073800                                                                          
073900 GF-FELMED-ERR-PF11-AND-NO-DATA SECTION.                                  
074100                                                                          
074200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
074300       CALL WMEDKONV USING MED-WMEDAREA                                   
074400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
074500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
074600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
074700       MOVE NEJ TO INDATA-SW                                              
074800       MOVE JA TO TABELL-SW                                               
074900     .                                                                    
075000                                                                          
075100     EJECT                                                                
075200                                                                          
075300 GG-INDATA-FEL SECTION.                                                   
075500                                                                          
075600     IF NY-TABELL                                                         
075700       CONTINUE                                                           
075800     ELSE                                                                 
075900       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
076000       CALL WMEDKONV USING MED-WMEDAREA                                   
076100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
076200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
076300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
076400       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                               
076500     END-IF                                                               
076600     .                                                                    
076700                                                                          
076800     EJECT                                                                
076900                                                                          
077000 GH-INDATA-SW-AER-K SECTION.                                              
077100                                                                          
077200     ADD +1 TO 4452-RAD-IX                                                
077300     PERFORM UNTIL MID-KVPTSORT(4452-RAD-IX) = ALL '+'                    
077400             OR 4452-RAD-IX > 9                                           
077500       MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-ATTR(4452-RAD-IX)           
077600       ADD +1 TO 4452-RAD-IX                                              
077700     END-PERFORM                                                          
077800     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
077900     CALL WMEDKONV USING MED-WMEDAREA                                     
078000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
078100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
078200     PERFORM MFS-ROER-EJ-FAELT-UT                                         
078300     PERFORM MFS-ROER-EJ-FAELT-IN                                         
078400     .                                                                    
078500                                                                          
078600     EJECT                                                                
078700                                                                          
078800 GI-INDATA-SW-AER-F SECTION.                                              
079000                                                                          
079100     MOVE '764' TO MED-IDMFSFEL                                           
079200**************** PLATS FINNS EJ *******************************           
079300     CALL WMEDKONV USING MED-WMEDAREA                                     
079400     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
079500     PERFORM MFS-ROER-EJ-FAELT-UT                                         
079600     PERFORM MFS-RENSA-FAELT-IN                                           
079700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
079800     .                                                                    
079900                                                                          
080000     EJECT                                                                
080100                                                                          
080200 GJ-KOLLA-JA-NEJ-SW SECTION.                                              
080300                                                                          
080400     IF GODK-SVAR  OR                                                     
080500        SVAR-SW = 'N'                                                     
080600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-JA-NEJ-SW-ATTR                    
080700     ELSE                                                                 
080800       MOVE MFS-ALFA-FAELT-FEL TO MOD-JA-NEJ-SW-ATTR                      
080900       MOVE MFS-ROER-EJ-FAELT  TO MOD-JA-NEJ-SW                           
081000       MOVE 'L' TO INDATA-SW                                              
081100     END-IF                                                               
081200     MOVE +1 TO 4452-RAD-IX                                               
081300     PERFORM 10 TIMES                                                     
081400       IF MID-KVPTSORT(4452-RAD-IX) NOT = ALL '+'                         
081500         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
081600                      MOD-KVPTSORT-ATTR(4452-RAD-IX)                      
081700         MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT(4452-RAD-IX)              
081800       END-IF                                                             
081900     ADD +1 TO 4452-RAD-IX                                                
082000     END-PERFORM                                                          
082100     .                                                                    
082200                                                                          
082300     EJECT                                                                
082400                                                                          
082500 GK-INDATA-SW-AER-L SECTION.                                              
082700                                                                          
082800     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
082900     CALL WMEDKONV USING MED-WMEDAREA                                     
083000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
083100     PERFORM MFS-ROER-EJ-FAELT-UT                                         
083200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
083300     .                                                                    
083400                                                                          
083500     EJECT                                                                
083600                                                                          
083700 H-UPPDATERA SECTION.                                                     
083800                                                                          
083900     IF NO-INPUT    AND MID-KDSORT-IN = ALL '+'  AND                      
084000          KVPTSORT-IN-NUM > ZERO                                          
084100       PERFORM HD-SAETT-INDEX                                             
084200       PERFORM HH-TA-BORT-RAD                                             
084300     ELSE                                                                 
084400       SET MID-NUM-IX TO +1                                               
084500       SET XXKI-4452-Y-IX TO +1                                           
084600       IF MID-KDSORT-IN NOT = ALL '+'                                     
084700         IF NO-INPUT                                                      
084800           PERFORM HA-UPPDATERA-KDSORT                                    
084900           MOVE JA TO FIRST-TIME-SW                                       
085000           PERFORM MFS-RENSA-FAELT-IN                                     
085100         END-IF                                                           
085200       END-IF                                                             
085300                                                                          
085400       IF NOT FIRST-TIME                                                  
085500         IF XXKI-4452-KVPTSORT-X(XXKI-4452-Y-IX) = MAX-VARDE AND          
085600                      KVPTSORT-IN-NUM = ZERO                              
085700**** TEST --------                                                        
085800            IF XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) = MAX-VARDE           
085900              PERFORM HB-UPPDATERA-FOERSTA-PA-X-AXEL                      
086000            ELSE                                                          
086100              PERFORM HE-FLYTTA-TILL-TABELL                               
086200              MOVE JA TO FIRST-TIME-SW                                    
086300            END-IF                                                        
086400**** SLUT TEST --------                                                   
086500         ELSE                                                             
086600           IF XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) = MAX-VARDE AND        
086700                        KVPTSORT-IN-NUM > ZERO                            
086900             PERFORM HC-UPPDATERA-FOERSTA-PA-Y-AXEL                       
087100           END-IF                                                         
087200         END-IF                                                           
087300           IF MID-KDSORT-IN NOT = ALL '+'                                 
087400               PERFORM HA-UPPDATERA-KDSORT                                
087500           END-IF                                                         
087600       END-IF                                                             
087700                                                                          
087800       IF NOT FIRST-TIME                                                  
087900         PERFORM HD-SAETT-INDEX                                           
088000         IF XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) =                        
088100                  KVPTSORT-IN-NUM                                         
088300           PERFORM HG-AENDRA-LAGG-TILL-TID                                
088500         ELSE                                                             
088600           PERFORM HE-FLYTTA-TILL-TABELL                                  
088700           IF INDATA-OK                                                   
088800             PERFORM HF-FLYTTA-TILL-XXKI-4452                             
088900           END-IF                                                         
089000         END-IF                                                           
089100       END-IF                                                             
089200                                                                          
089300       IF INDATA-SW = 'Ä'                                                 
089400         MOVE JA TO INDATA-SW                                             
089500       END-IF                                                             
089600       IF INDATA-OK  OR INDATA-SW = 'W'                                   
089800         IF INDATA-SW = 'W'                                               
090000            CONTINUE                                                      
090100         ELSE                                                             
090300           PERFORM IMS-REPL-XXKI                                          
090400           PERFORM S01-LAES-VISA-INFO                                     
090500         END-IF                                                           
090700         IF AENDRING                                                      
090900           MOVE '779' TO MED-IDMFSINF                                     
091000         ELSE                                                             
091200           IF AENDRING-SW = 'F'                                           
091400             MOVE '409' TO MED-IDMFSFEL                                   
091500             CALL WMEDKONV USING MED-WMEDAREA                             
091600             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
091700             MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                         
091800             PERFORM MFS-ROER-EJ-FAELT-IN                                 
091900             PERFORM MFS-ROER-EJ-FAELT-UT                                 
092000           ELSE                                                           
092200             IF INDATA-SW NOT = 'Ö'                                       
092300               MOVE INF-UPDATE-DONE TO MED-IDMFSINF                       
092400             END-IF                                                       
092500           END-IF                                                         
092600         END-IF                                                           
092800         IF AENDRING-SW NOT = 'F'                                         
093000           CALL WMEDKONV USING MED-WMEDAREA                               
093100           MOVE MED-MFSINF TO MOD-TEMFSINF                                
093200           PERFORM MFS-RENSA-FAELT-IN                                     
093300         END-IF                                                           
093400       ELSE                                                               
093500          PERFORM MFS-ROER-EJ-FAELT-IN                                    
093600          PERFORM MFS-ROER-EJ-FAELT-UT                                    
093700       END-IF                                                             
093800     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200 HA-UPPDATERA-KDSORT SECTION.                                             
094400                                                                          
094500     IF INDATA-OK                                                         
094600       IF KVPTSORT-IN-NUM = ZERO                                          
094700         MOVE MID-KDSORT-IN TO MOD-KDSORT-X                               
094800                               XXKI-4452-KDSORT-X                         
094900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSORT-X-ATTR                  
095000       ELSE                                                               
095100         MOVE MID-KDSORT-IN TO MOD-KDSORT-Y                               
095200                               XXKI-4452-KDSORT-Y                         
095300         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSORT-Y-ATTR                  
095400       END-IF                                                             
095500     END-IF                                                               
095600                                                                          
095700     .                                                                    
095800     EJECT                                                                
095900                                                                          
096000 HB-UPPDATERA-FOERSTA-PA-X-AXEL SECTION.                                  
096100                                                                          
096200     SET XXKI-4452-Y-IX TO +1                                             
096300     SET MID-NUM-IX TO +1                                                 
096400     MOVE JA TO FIRST-TIME-SW                                             
096500     MOVE +10 TO X-INDX                                                   
096600     MOVE +10 TO Y-INDX                                                   
096700                                                                          
096800     PERFORM UNTIL MID-KVPTSORT(X-INDX) NOT = ALL '+'                     
096900       SUBTRACT +1 FROM X-INDX                                            
097000     END-PERFORM                                                          
097100                                                                          
097200     IF X-INDX NOT = +1                                                   
097300       SUBTRACT +1 FROM X-INDX                                            
097400     END-IF                                                               
097500                                                                          
097600     PERFORM UNTIL X-INDX = +1                                            
097700       IF MID-KVPTSORT(X-INDX) = ALL '+'                                  
097800         MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-ATTR(X-INDX)              
097900         MOVE NEJ TO INDATA-SW                                            
098000       END-IF                                                             
098100       SUBTRACT +1 FROM X-INDX                                            
098200     END-PERFORM                                                          
098300     IF INDATA-FEL                                                        
098400       MOVE '413' TO MED-IDMFSFEL                                         
098500*****  INFORMATION SAKNAS *******************                             
098600       CALL WMEDKONV USING MED-WMEDAREA                                   
098700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
098800       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                               
098900*      PERFORM MFS-ROER-EJ-FAELT-UT                                       
099000*      PERFORM MFS-ROER-EJ-FAELT-IN                                       
099100     END-IF                                                               
099200*---JEF 910218                                                            
099300     IF MID-KVPTSORT(10) NOT = ALL '+'                                    
099400        MOVE 'B' TO INDATA-SW                                             
099500     END-IF                                                               
099600*---JEF 910218                                                            
099700     MOVE +1 TO X-INDX                                                    
099800     IF GODK-SVAR AND HELP-MID-AREA(MID-NUM-IX) > ZERO  AND               
099900                      INDATA-OK                                           
100000       PERFORM UNTIL XXKI-4452-Y-IX > 9                                   
100100         IF HELP-MID-AREA(MID-NUM-IX) > ZERO                              
100200           MOVE HELP-MID-AREA(MID-NUM-IX) TO                              
100300                             XXKI-4452-KVPTSORT-X(XXKI-4452-Y-IX)         
100400           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
100500                          MOD-KVPTSORT-X-ATTR(X-INDX)                     
100600         END-IF                                                           
100700         SET XXKI-4452-Y-IX UP BY +1                                      
100800         SET MID-NUM-IX UP BY +1                                          
100900         ADD +1 TO X-INDX                                                 
101000       END-PERFORM                                                        
101010                                                                          
101100       MOVE +10 TO X-INDX                                                 
101200       PERFORM UNTIL MID-KVPTSORT(X-INDX) NOT = ALL '+'                   
101300         SUBTRACT +1 FROM X-INDX                                          
101400       END-PERFORM                                                        
101500       ADD +1 TO X-INDX                                                   
101600       MOVE MAX-VARDE TO                                                  
101700                 XXKI-4452-KVPTSORT-X(X-INDX)                             
101800*---JEF 910218                                                            
101900       MOVE XXKI-4452-KVPTID(1, 1) TO                                     
102000                   XXKI-4452-KVPTID(1, X-INDX)                            
102100       MOVE ZERO TO XXKI-4452-KVPTID(1, 1)                                
102200*---JEF 910218 SLUT                                                       
102300     ELSE                                                                 
102400       IF INDATA-OK                                                       
102500         MOVE NEJ TO INDATA-SW                                            
102600         MOVE '777' TO MED-IDMFSFEL                                       
102700*****    UPPDATERING EJ TILLÅTEN***************                           
102800         CALL WMEDKONV USING MED-WMEDAREA                                 
102900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
103000         MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                             
103100*        PERFORM MFS-ROER-EJ-FAELT-UT                                     
103200*        PERFORM MFS-ROER-EJ-FAELT-IN                                     
103300       ELSE                                                               
103400         IF INDATA-SW = 'B'                                               
103500           MOVE NEJ TO INDATA-SW                                          
103600           MOVE '777' TO MED-IDMFSFEL                                     
103700********** UPPDATERING EJ TILLÅTEN***************                         
103800           CALL WMEDKONV USING MED-WMEDAREA                               
103900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
104000           MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                           
104100           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-ATTR(10)                
104200           MOVE JA TO MOD-JA-NEJ-SW                                       
104300         END-IF                                                           
104301       END-IF                                                             
104310     END-IF                                                               
104400                                                                          
104500     .                                                                    
104600     EJECT                                                                
104700 HC-UPPDATERA-FOERSTA-PA-Y-AXEL SECTION.                                  
104900                                                                          
105000     MOVE JA TO FIRST-TIME-SW                                             
105100     MOVE +10 TO Y-INDX                                                   
105200     MOVE +1 TO X-INDX                                                    
105300     MOVE +1 TO SPAR-INDX                                                 
105400     SUBTRACT 1 FROM MAX-X-IX                                             
105500     SET XXKI-4452-Y-IX TO +1                                             
105600     SET XXKI-4452-X-IX TO +1                                             
105700     SET MID-NUM-IX TO +1                                                 
105800     PERFORM UNTIL MID-KVPTSORT(Y-INDX) NOT = ALL '+'                     
105900        SUBTRACT +1 FROM Y-INDX                                           
106000     END-PERFORM                                                          
106100     PERFORM S03-KOLLA-STORLEK                                            
106200     IF Y-INDX > 4452-RAD-IX                                              
106300       MOVE '007' TO MED-IDMFSFEL                                         
106400******************** OTILLÅTEN UPPDATERING *******************            
106500       CALL WMEDKONV USING MED-WMEDAREA                                   
106600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
106610                                                                          
106700       MOVE +1 TO Y-INDX                                                  
106800       PERFORM UNTIL XXKI-4452-KVPTSORT-X(Y-INDX) = MAX-VARDE             
106900         ADD +1 TO Y-INDX                                                 
107000       END-PERFORM                                                        
107010                                                                          
107100       ADD +1 TO Y-INDX                                                   
107200       PERFORM UNTIL Y-INDX > +9                                          
107300         IF MID-KVPTSORT(Y-INDX) NOT = ALL '+'                            
107400            MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-ATTR(Y-INDX)           
107500         END-IF                                                           
107600         ADD +1 TO Y-INDX                                                 
107700       END-PERFORM                                                        
107800       MOVE NEJ TO INDATA-SW                                              
108200     ELSE                                                                 
108500       MOVE +10 TO Y-INDX                                                 
108600       PERFORM UNTIL MID-KVPTSORT(X-INDX) NOT = ALL '+'                   
108700         ADD +1 TO X-INDX                                                 
108800         SET XXKI-4452-X-IX UP BY +1                                      
108900       END-PERFORM                                                        
109000                                                                          
109100       ADD +1 TO MAX-X-IX                                                 
109200       IF KVPTSORT-IN-NUM NOT = MAX-VARDE                                 
109400         IF XXKI-4452-KVPTID(SPAR-INDX, X-INDX) > ZERO                    
109500           PERFORM UNTIL X-INDX > 4452-RAD-IX OR X-INDX > 10              
109600             MOVE XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX)        
109700             TO                                                           
109800             XXKI-4452-KVPTID(XXKI-4452-Y-IX + 1, XXKI-4452-X-IX)         
109900             SET XXKI-4452-X-IX UP BY +1                                  
110000             ADD +1 TO X-INDX                                             
110100           END-PERFORM                                                    
110200         END-IF                                                           
110300         SET XXKI-4452-X-IX TO +1                                         
110400         MOVE +1 TO X-INDX                                                
110500*---JF                                                                    
110600         MOVE KVPTSORT-IN-NUM TO                                          
110700                              XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX)        
110800         MOVE MAX-VARDE TO                                                
110900                    XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX + 1)              
111000         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVPTSORT-Y-ATTR(Y-INDX)        
111100       END-IF                                                             
111110                                                                          
111200       MOVE +1 TO X-INDX                                                  
111300       PERFORM UNTIL  X-INDX > +10                                        
111400         IF MID-KVPTSORT(X-INDX) NOT = ALL '+'                            
111500           MOVE HELP-MID-AREA(X-INDX)  TO                                 
111600              XXKI-4452-KVPTID(SPAR-INDX, X-INDX)                         
111700            MOVE MFS-ADD-LYS-UPP-FAELT TO                                 
111800                MOD-KVPTID-ATTR(Y-INDX, X-INDX)                           
111900         ELSE                                                             
112000            IF XXKI-4452-KVPTSORT-X(X-INDX) = MAX-VARDE AND               
112100               MID-KVPTSORT-IN NOT = '9999.9' AND                         
112200               XXKI-4452-KVPTSORT-Y(X-INDX) NOT = MAX-VARDE               
112300               MOVE +10 TO                                                
112400                XXKI-4452-KVPTID(SPAR-INDX + 1, X-INDX)                   
112500               MOVE +0  TO                                                
112600                XXKI-4452-KVPTID(SPAR-INDX, X-INDX)                       
112700            END-IF                                                        
112800         END-IF                                                           
112900*        SET MID-NUM-IX UP BY +1                                          
113000         ADD +1 TO X-INDX                                                 
113100       END-PERFORM                                                        
113200     END-IF                                                               
113300                                                                          
113400     MOVE +1          TO  X-INDX                                          
113500     PERFORM UNTIL XXKI-4452-KVPTSORT-X(X-INDX) = MAX-VARDE               
113510                OR X-INDX = 10                                            
113600        ADD +1 TO X-INDX                                                  
113700     END-PERFORM                                                          
113800                                                                          
113900     ADD +1 TO X-INDX                                                     
114000                                                                          
114100     PERFORM UNTIL   X-INDX > 4452-RAD-IX                                 
114200       MOVE +0          TO                                                
114300          XXKI-4452-KVPTID(SPAR-INDX, X-INDX)                             
114400        ADD +1 TO X-INDX                                                  
114500     END-PERFORM                                                          
114700     .                                                                    
114800     EJECT                                                                
114900 HD-SAETT-INDEX SECTION.                                                  
115000                                                                          
115100     SET XXKI-4452-Y-IX TO +1                                             
115200     MOVE +1 TO X-INDX                                                    
115300     MOVE +1 TO SPAR-INDX                                                 
115400     IF KVPTSORT-IN-NUM = ZERO                                            
115500       IF MID-KVPTSORT(XXKI-4452-Y-IX) = ALL '+'                          
115510                                                                          
115600         PERFORM UNTIL MID-KVPTSORT(X-INDX) NOT = ALL '+'                 
115700           ADD +1 TO X-INDX                                               
115800           ADD +1 TO SPAR-INDX                                            
115900         END-PERFORM                                                      
116000       END-IF                                                             
116100     ELSE                                                                 
116200         PERFORM UNTIL XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) =             
116300            KVPTSORT-IN-NUM                                               
116400              OR XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) >                   
116500                KVPTSORT-IN-NUM                                           
116600                                                                          
116700           SET XXKI-4452-Y-IX UP BY +1                                    
116800           ADD +1 TO SPAR-INDX                                            
116900         END-PERFORM                                                      
117000     END-IF                                                               
117100     PERFORM HDA-SAETT-MOD-INDX                                           
117200     .                                                                    
117300     EJECT                                                                
117400 HDA-SAETT-MOD-INDX SECTION.                                              
117600                                                                          
117700     EVALUATE SPAR-INDX                                                   
117800       WHEN 1                                                             
117900         MOVE 10 TO MOD-INDX                                              
118000       WHEN 2                                                             
118100         MOVE  9 TO MOD-INDX                                              
118200       WHEN 3                                                             
118300         MOVE  8 TO MOD-INDX                                              
118400       WHEN 4                                                             
118500         MOVE  7 TO MOD-INDX                                              
118600       WHEN 5                                                             
118700         MOVE  6 TO MOD-INDX                                              
118800       WHEN 6                                                             
118900         MOVE  5 TO MOD-INDX                                              
119000       WHEN 7                                                             
119100         MOVE  4 TO MOD-INDX                                              
119200       WHEN 8                                                             
119300         MOVE  3 TO MOD-INDX                                              
119400       WHEN 9                                                             
119500         MOVE  2 TO MOD-INDX                                              
119600       WHEN 10                                                            
119700         MOVE  1 TO MOD-INDX                                              
119800       WHEN OTHER                                                         
119900         CALL FELLOG                                                      
120000     END-EVALUATE                                                         
120100     .                                                                    
120200     EJECT                                                                
120300 HE-FLYTTA-TILL-TABELL SECTION.                                           
120500                                                                          
120600     SET XXKI-4452-Y-IX TO +1                                             
120700     SET MID-NUM-IX TO +1                                                 
120800     SET HELP-Y-IX TO +1                                                  
120900     MOVE +1 TO Y-INDX                                                    
121000     MOVE +10 TO X-INDX                                                   
121100                                                                          
121200     IF KVPTSORT-IN-NUM = ZERO                                            
121300       IF GODK-SVAR                                                       
121400         PERFORM HEA-FLYTTA-X-AXEL-TILL-TABELL                            
121500       ELSE                                                               
121600         PERFORM HEAB-AENDRA-X-AXEL                                       
121700       END-IF                                                             
121800     ELSE                                                                 
121900       PERFORM S03-KOLLA-STORLEK                                          
121910                                                                          
122000       ADD +1 TO 4452-RAD-IX                                              
122100       PERFORM UNTIL 4452-RAD-IX > 9                                      
122200         IF MID-KVPTSORT(4452-RAD-IX) NOT = ALL '+'                       
122300           MOVE NEJ TO INDATA-SW                                          
122400          MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-ATTR(4452-RAD-IX)        
122500           MOVE '007' TO MED-IDMFSFEL                                     
122600********************     OTILLÅTEN UPPDATERING ***************            
122700           CALL WMEDKONV USING MED-WMEDAREA                               
122800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
122900         END-IF                                                           
123000         ADD +1 TO 4452-RAD-IX                                            
123100       END-PERFORM                                                        
123200       IF INDATA-OK                                                       
123300         PERFORM HEB-FLYTTA-Y-AXEL-TILL-TABELL                            
123400       ELSE                                                               
123500         PERFORM MFS-ROER-EJ-FAELT-IN                                     
123600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
123700       END-IF                                                             
123800     END-IF                                                               
124000     .                                                                    
124100     EJECT                                                                
124200 HEA-FLYTTA-X-AXEL-TILL-TABELL SECTION.                                   
124300                                                                          
124400     PERFORM HD-SAETT-INDEX                                               
124500     MOVE NEJ TO MOD-JA-NEJ-SW                                            
124600     PERFORM S03-KOLLA-STORLEK                                            
124700     IF 4452-RAD-IX NOT > 10                                              
124710                                                                          
124800       MOVE +1 TO HELP-INDX                                               
124900       SUBTRACT 1 FROM SPAR-INDX                                          
125000       PERFORM SPAR-INDX TIMES                                            
125100                                                                          
125200         MOVE XXKI-4452-KVPTSORT-X(XXKI-4452-Y-IX) TO                     
125300                           HELP-KVPTSORT-X(HELP-Y-IX)                     
125400         SET XXKI-4452-Y-IX UP BY +1                                      
125500         SET XXKI-4452-X-IX UP BY +1                                      
125600         SET HELP-Y-IX UP BY +1                                           
125700         ADD +1 TO Y-INDX                                                 
125800         ADD +1 TO HELP-INDX                                              
125900       END-PERFORM                                                        
126000******************************************************************        
126100       SET XXKI-4452-Y-IX TO +1                                           
126200       SET HELP-Y-IX TO +1                                                
126300                                                                          
126400******************************************************************        
126500       PERFORM UNTIL XXKI-4452-Y-IX > 10                                  
126600         SET XXKI-4452-X-IX TO +1                                         
126700         SET HELP-X-IX TO +1                                              
126800         PERFORM SPAR-INDX TIMES                                          
126900           MOVE XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX)          
127000                        TO HELP-KVPTID(HELP-Y-IX, HELP-X-IX)              
127100           SET XXKI-4452-X-IX UP BY +1                                    
127200           SET HELP-X-IX UP BY +1                                         
127300         END-PERFORM                                                      
127400         SET XXKI-4452-Y-IX UP BY +1                                      
127500         SET HELP-Y-IX UP BY +1                                           
127600       END-PERFORM                                                        
127700                                                                          
127800******************************************************************        
127900       MOVE SPAR-INDX TO 4452-RAD-IX                                      
128000       IF SPAR-INDX < 10                                                  
128100         COMPUTE SPAR-INDX = SPAR-INDX + 1                                
128200       END-IF                                                             
128300       PERFORM HEC-KOLLA-DUBLETT                                          
128400       IF HELP-MID-AREA(4452-RAD-IX) <                                    
128500          XXKI-4452-KVPTSORT-X(SPAR-INDX) AND INDATA-SW = JA              
128600           MOVE HELP-MID-AREA(SPAR-INDX) TO                               
128700                             HELP-KVPTSORT-X(SPAR-INDX)                   
128800           ADD +1 TO SPAR-INDX                                            
128900           MOVE XXKI-4452-KVPTSORT-X(4452-RAD-IX) TO                      
129000                             HELP-KVPTSORT-X(SPAR-INDX)                   
129100           SUBTRACT 1 FROM SPAR-INDX                                      
129200           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
129300                        MOD-KVPTSORT-X-ATTR(SPAR-INDX)                    
129400           MOVE +1 TO 4452-RAD-IX                                         
129500           ADD +1 TO HELP-INDX                                            
129600           MOVE SPAR-INDX TO MAX-X-IX                                     
129700******************************************************************        
129800           PERFORM 10 TIMES                                               
129900             MOVE SPAR-INDX TO X-INDX                                     
130000             COMPUTE Z-INDX = X-INDX + 1                                  
130100                                                                          
130200             MOVE XXKI-4452-KVPTSORT-X(Y-INDX) TO                         
130300                               HELP-KVPTSORT-X(HELP-INDX)                 
130400             MOVE ZERO TO XXKI-4452-KVPTSORT-X(Y-INDX)                    
130500             PERFORM UNTIL X-INDX > 10                                    
130600               MOVE XXKI-4452-KVPTID(4452-RAD-IX, X-INDX) TO              
130700                                 HELP-KVPTID(4452-RAD-IX, Z-INDX)         
130800               IF X-INDX = SPAR-INDX                                      
130900                MOVE ZERO TO XXKI-4452-KVPTID(4452-RAD-IX, X-INDX)        
131000               END-IF                                                     
131100               ADD 1 TO X-INDX                                            
131200               ADD 1 TO Z-INDX                                            
131300                                                                          
131400             END-PERFORM                                                  
131500             SET XXKI-4452-X-IX UP BY +1                                  
131600             ADD 1 TO 4452-RAD-IX                                         
131700             ADD 1 TO Y-INDX                                              
131800             ADD 1 TO HELP-INDX                                           
131900             ADD 1 TO MAX-X-IX                                            
132000           END-PERFORM                                                    
132100           PERFORM HF-FLYTTA-TILL-XXKI-4452                               
132200******************************************************************        
132300       ELSE                                                               
132400         PERFORM HEAA-FELMED-738                                          
132500       END-IF                                                             
132600     ELSE                                                                 
132700       MOVE NEJ TO INDATA-SW                                              
132800       MOVE '777' TO MED-IDMFSFEL                                         
132900*****  UPPDATERING EJ TILLÅTEN***************                             
133000       CALL WMEDKONV USING MED-WMEDAREA                                   
133100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
133200       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                               
133300       PERFORM MFS-RENSA-FAELT-IN                                         
133400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
133500     END-IF                                                               
133600     .                                                                    
133700     EJECT                                                                
133800                                                                          
133900 HEAA-FELMED-738 SECTION.                                                 
134100                                                                          
134200     MOVE NEJ TO INDATA-SW                                                
134300     MOVE '738' TO MED-IDMFSFEL                                           
134400**********FELAKTIGA INTERVALLUPPGIFTER *****************                  
134500     CALL WMEDKONV USING MED-WMEDAREA                                     
134600     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
134700     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSINF                                
134800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
134900     MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT-IN                            
135000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPTSORT-IN-ATTR                   
135010                                                                          
135100     MOVE +1 TO 4452-RAD-IX                                               
135200     PERFORM UNTIL MID-KVPTSORT(4452-RAD-IX) NOT = ALL '+'                
135300             OR 4452-RAD-IX > 9                                           
135400       ADD +1 TO 4452-RAD-IX                                              
135500     END-PERFORM                                                          
135600     MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT(4452-RAD-IX)                  
135700     MOVE MFS-NUM-FAELT-FEL         TO                                    
135800                  MOD-KVPTSORT-ATTR(4452-RAD-IX)                          
135900                                                                          
136000     .                                                                    
136100     EJECT                                                                
136200 HEAB-AENDRA-X-AXEL SECTION.                                              
136400                                                                          
136500     MOVE JA TO INDATA-SW                                                 
136600     MOVE +1  TO X-INDX                                                   
136700                                                                          
136800     PERFORM UNTIL MID-KVPTSORT(X-INDX) NOT = ALL '+'  OR                 
136900                                X-INDX > 9                                
137000       IF XXKI-4452-KVPTSORT-X(X-INDX) = MAX-VARDE                        
137100          MOVE NEJ TO INDATA-SW                                           
137200       END-IF                                                             
137300       ADD +1 TO X-INDX                                                   
137400     END-PERFORM                                                          
137500                                                                          
137600     IF MID-KVPTSORT(X-INDX) =  ALL '0'                                   
137700                                                                          
137800       MOVE X-INDX TO L-INDX                                              
137900       PERFORM UNTIL L-INDX > 9                                           
138000         IF MID-KVPTSORT(L-INDX) NOT = ALL '+'                            
138100           IF MID-KVPTSORT(L-INDX) = ALL '0' AND                          
138200              INDATA-OK AND                                               
138300              XXKI-4452-KVPTSORT-X(L-INDX) NOT = MAX-VARDE                
138400              CONTINUE                                                    
138500            ELSE                                                          
138600              MOVE NEJ TO INDATA-SW                                       
138700              MOVE MFS-ALFA-FAELT-FEL TO                                  
138800                              MOD-KVPTSORT-ATTR(L-INDX)                   
138900            END-IF                                                        
139000         END-IF                                                           
139100         ADD +1 TO L-INDX                                                 
139200       END-PERFORM                                                        
139300       IF INDATA-OK                                                       
139400         PERFORM HEAD-TA-BORT-KOLUMN                                      
139500       ELSE                                                               
139600         MOVE '007' TO MED-IDMFSFEL                                       
139700****************** OTILLÅTEN UPPDATERING******************                
139800         CALL WMEDKONV USING MED-WMEDAREA                                 
139900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
140000         MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                             
140100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
140200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
140300       END-IF                                                             
140500     ELSE                                                                 
140600       PERFORM UNTIL MID-KVPTSORT(X-INDX) = ALL '+' OR                    
140700                                  X-INDX > 10                             
140800                                                                          
140900         IF HELP-MID-AREA(X-INDX) <                                       
141000            XXKI-4452-KVPTSORT-X(X-INDX + 1) AND                          
141300            XXKI-4452-KVPTSORT-X(X-INDX) NOT = MAX-VARDE                  
141400            ADD +1 TO X-INDX                                              
141500         ELSE                                                             
141600           MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-ATTR(X-INDX)            
141700           MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT(X-INDX)                 
141800           ADD +1 TO X-INDX                                               
141900           MOVE NEJ TO INDATA-SW                                          
142000         END-IF                                                           
142100       END-PERFORM                                                        
142200                                                                          
142300       IF INDATA-OK                                                       
142310                                                                          
142400         MOVE +1 TO X-INDX                                                
142500         PERFORM UNTIL MID-KVPTSORT(X-INDX) NOT = ALL '+' OR              
142600                                    X-INDX > 9                            
142700           ADD +1 TO X-INDX                                               
142800         END-PERFORM                                                      
142900                                                                          
143000         PERFORM UNTIL MID-KVPTSORT(X-INDX) = ALL '+' OR                  
143100                                  X-INDX > 10                             
143200           MOVE ZERO TO XXKI-4452-KVPTSORT-X(X-INDX)                      
143300           MOVE HELP-MID-AREA(X-INDX) TO                                  
143400                      XXKI-4452-KVPTSORT-X(X-INDX)                        
143500           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
143600                      MOD-KVPTSORT-X-ATTR(X-INDX)                         
143700           ADD +1 TO X-INDX                                               
143800         END-PERFORM                                                      
143900       END-IF                                                             
144000       IF INDATA-FEL                                                      
144100         PERFORM HEAC-FELMED-738                                          
144200       ELSE                                                               
144300         MOVE 'Ä' TO INDATA-SW                                            
144400       END-IF                                                             
144500     END-IF                                                               
144600     .                                                                    
144700                                                                          
144800     EJECT                                                                
144900                                                                          
145000 HEAC-FELMED-738 SECTION.                                                 
145200                                                                          
145300     MOVE '738' TO MED-IDMFSFEL                                           
145400******    FELAKTIGA INTERVALLUPPGIFTER *****************                  
145500     CALL WMEDKONV USING MED-WMEDAREA                                     
145600     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
145700     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSINF                                
145800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
145900     MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT-IN                            
146000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPTSORT-IN-ATTR                   
146100     .                                                                    
146200                                                                          
146300     EJECT                                                                
146400                                                                          
146500 HEAD-TA-BORT-KOLUMN SECTION.                                             
146600                                                                          
146700     MOVE +1 TO X-INDX                                                    
146800                L-INDX                                                    
146900                Y-INDX                                                    
147000     PERFORM 10 TIMES                                                     
147100       IF MID-KVPTSORT(X-INDX) = ALL '+'                                  
147200         MOVE XXKI-4452-KVPTSORT-X(X-INDX) TO                             
147300                                  HELP-KVPTSORT-X(L-INDX)                 
147400         PERFORM 10 TIMES                                                 
147500           MOVE XXKI-4452-KVPTID(Y-INDX, X-INDX) TO                       
147600                           HELP-KVPTID(Y-INDX, L-INDX)                    
147700           ADD +1 TO Y-INDX                                               
147800         END-PERFORM                                                      
147900         MOVE +1 TO Y-INDX                                                
148000         ADD +1 TO L-INDX                                                 
148100       END-IF                                                             
148200       ADD +1 TO X-INDX                                                   
148300     END-PERFORM                                                          
148400     .                                                                    
148500     EJECT                                                                
148600                                                                          
148700 HEB-FLYTTA-Y-AXEL-TILL-TABELL SECTION.                                   
148900                                                                          
149000     PERFORM S03-KOLLA-STORLEK                                            
149100       PERFORM UNTIL XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) >               
149200            KVPTSORT-IN-NUM  OR XXKI-4452-Y-IX > 9                        
149300         SET XXKI-4452-X-IX TO +1                                         
149400         SET HELP-X-IX TO +1                                              
149500                                                                          
149600         MOVE XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) TO                     
149700                           HELP-KVPTSORT-Y(HELP-Y-IX)                     
149800         PERFORM 4452-RAD-IX TIMES                                        
149900           MOVE XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX)          
150000           TO      HELP-KVPTID(HELP-Y-IX, HELP-X-IX)                      
150100           SET XXKI-4452-X-IX UP BY +1                                    
150200           SET HELP-X-IX UP BY +1                                         
150300         END-PERFORM                                                      
150400         SET XXKI-4452-Y-IX UP BY +1                                      
150500         SET HELP-Y-IX UP BY +1                                           
150600         ADD +1 TO Y-INDX                                                 
150700         SUBTRACT 1 FROM X-INDX                                           
150800       END-PERFORM                                                        
150900                                                                          
151100     MOVE KVPTSORT-IN-NUM TO                                              
151200                       HELP-KVPTSORT-Y(SPAR-INDX)                         
151300     MOVE +1 TO Z-INDX                                                    
151400     MOVE MFS-ADD-LYS-UPP-FAELT TO                                        
151500                    MOD-KVPTSORT-Y-ATTR(X-INDX)                           
151600     PERFORM    UNTIL MID-NUM-IX > 4452-RAD-IX OR                         
151700                MID-NUM-IX > 10                                           
151800       MOVE HELP-MID-AREA(Z-INDX) TO                                      
151900                  HELP-KVPTID(Y-INDX, Z-INDX)                             
152000       IF HELP-MID-AREA(Z-INDX) > ZERO                                    
152100         MOVE MFS-ADD-LYS-UPP-FAELT TO                                    
152200                  MOD-KVPTID-ATTR(MOD-INDX, Z-INDX)                       
152300       END-IF                                                             
152400       ADD +1 TO Z-INDX                                                   
152500       SUBTRACT 1 FROM X-INDX                                             
152600       SET MID-NUM-IX UP BY +1                                            
152700     END-PERFORM                                                          
152800                                                                          
153000     PERFORM UNTIL XXKI-4452-Y-IX > 9                                     
153100                                                                          
153200       IF XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) > ZERO                     
153300         MOVE XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) TO                     
153400                           HELP-KVPTSORT-Y(HELP-Y-IX + 1)                 
153500         SET XXKI-4452-X-IX TO +1                                         
153600         SET HELP-X-IX TO +1                                              
153700         PERFORM 4452-RAD-IX TIMES                                        
153800           MOVE XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX)          
153900           TO      HELP-KVPTID(HELP-Y-IX + 1, HELP-X-IX)                  
154000           SET XXKI-4452-X-IX UP BY +1                                    
154100           SET HELP-X-IX UP BY +1                                         
154200         END-PERFORM                                                      
154300       END-IF                                                             
154400       SET XXKI-4452-Y-IX UP BY +1                                        
154500       SET HELP-Y-IX UP BY +1                                             
154600     END-PERFORM                                                          
154800     .                                                                    
154900     EJECT                                                                
155000 HEC-KOLLA-DUBLETT SECTION.                                               
155300                                                                          
155400     MOVE +1 TO 4452-RAD-IX                                               
155500     PERFORM UNTIL XXKI-4452-KVPTSORT-X(4452-RAD-IX) = MAX-VARDE          
155600               OR  4452-RAD-IX > 10                                       
155700       IF MID-KVPTSORT(4452-RAD-IX) NOT = ALL '+'                         
155800       IF 4452-RAD-IX = +1                                                
155900         IF HELP-MID-AREA(4452-RAD-IX) =                                  
156000            XXKI-4452-KVPTSORT-X(4452-RAD-IX) OR                          
156100            HELP-MID-AREA(4452-RAD-IX) NOT <                              
156200            XXKI-4452-KVPTSORT-X(4452-RAD-IX)                             
156300                                                                          
156400            MOVE NEJ TO INDATA-SW                                         
156500            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-JA-NEJ-SW-ATTR              
156600            MOVE MFS-ROER-EJ-FAELT TO MOD-JA-NEJ-SW                       
156700                                                                          
156800         END-IF                                                           
156900       ELSE                                                               
157000         IF HELP-MID-AREA(4452-RAD-IX) =                                  
157100            XXKI-4452-KVPTSORT-X(4452-RAD-IX)                             
157200            MOVE NEJ TO INDATA-SW                                         
157300            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-JA-NEJ-SW-ATTR              
157400            MOVE MFS-ROER-EJ-FAELT TO MOD-JA-NEJ-SW                       
157500         ELSE                                                             
157600           IF HELP-MID-AREA(4452-RAD-IX) <                                
157700              XXKI-4452-KVPTSORT-X(4452-RAD-IX) AND                       
157800              HELP-MID-AREA(4452-RAD-IX) >                                
157900                XXKI-4452-KVPTSORT-X(4452-RAD-IX - 1)                     
158000                CONTINUE                                                  
158100           ELSE                                                           
158200              MOVE NEJ TO INDATA-SW                                       
158300              MOVE MFS-ADD-LAES-IN-FAELT TO MOD-JA-NEJ-SW-ATTR            
158400              MOVE MFS-ROER-EJ-FAELT TO MOD-JA-NEJ-SW                     
158500           END-IF                                                         
158600         END-IF                                                           
158700       END-IF                                                             
158800       END-IF                                                             
158900       ADD +1 TO 4452-RAD-IX                                              
159000     END-PERFORM                                                          
159100                                                                          
159200     IF MID-KVPTSORT(4452-RAD-IX) NOT = ALL '+' AND INDATA-OK             
159300        PERFORM HECA-KOLLA-DUBLETT                                        
159400     END-IF                                                               
159500     .                                                                    
159600     EJECT                                                                
159700                                                                          
159800 HECA-KOLLA-DUBLETT SECTION.                                              
160000                                                                          
160100     MOVE +1 TO 4452-RAD-IX                                               
160200     PERFORM UNTIL XXKI-4452-KVPTSORT-X(4452-RAD-IX) = MAX-VARDE          
160300        ADD +1 TO 4452-RAD-IX                                             
160400     END-PERFORM                                                          
160500                                                                          
160600     IF MID-KVPTSORT(4452-RAD-IX) NOT = ALL '+'                           
160700        IF HELP-MID-AREA(4452-RAD-IX) < MAX-VARDE AND                     
160800           > XXKI-4452-KVPTSORT-X(4452-RAD-IX - 1)                        
160900            CONTINUE                                                      
161000        ELSE                                                              
161100            MOVE NEJ TO INDATA-SW                                         
161200            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-JA-NEJ-SW-ATTR              
161300            MOVE MFS-ROER-EJ-FAELT TO MOD-JA-NEJ-SW                       
161400        END-IF                                                            
161500     END-IF                                                               
161600     .                                                                    
161700     EJECT                                                                
161800                                                                          
161900 HF-FLYTTA-TILL-XXKI-4452 SECTION.                                        
162100                                                                          
162200     SET XXKI-4452-Y-IX TO +1                                             
162300     SET HELP-Y-IX TO +1                                                  
162400     PERFORM UNTIL XXKI-4452-Y-IX > 10                                    
162500                                                                          
162600       IF KVPTSORT-IN-NUM = ZERO                                          
162700         MOVE HELP-KVPTSORT-X(HELP-Y-IX) TO                               
162800                   XXKI-4452-KVPTSORT-X(XXKI-4452-Y-IX)                   
162900       ELSE                                                               
163000         MOVE HELP-KVPTSORT-Y(HELP-Y-IX) TO                               
163100                   XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX)                   
163200       END-IF                                                             
163300       SET XXKI-4452-Y-IX UP BY +1                                        
163400       SET HELP-Y-IX UP BY +1                                             
163500     END-PERFORM                                                          
163600     PERFORM HFA-FLYTTA-TID-TILL-XXKI-4452                                
163700     .                                                                    
163800     EJECT                                                                
163900 HFA-FLYTTA-TID-TILL-XXKI-4452 SECTION.                                   
164100                                                                          
164200     SET XXKI-4452-Y-IX TO +1                                             
164300     SET HELP-Y-IX TO +1                                                  
164400     PERFORM UNTIL XXKI-4452-Y-IX > 10                                    
164410                                                                          
164500       SET XXKI-4452-X-IX TO +1                                           
164600       SET HELP-X-IX TO +1                                                
164700       PERFORM UNTIL XXKI-4452-X-IX > 10                                  
164800           MOVE HELP-KVPTID(HELP-Y-IX, HELP-X-IX) TO                      
164900                XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX)          
165000         SET XXKI-4452-X-IX UP BY +1                                      
165100         SET HELP-X-IX UP BY +1                                           
165200       END-PERFORM                                                        
165300       SET XXKI-4452-Y-IX UP BY +1                                        
165400       SET HELP-Y-IX UP BY +1                                             
165500     END-PERFORM                                                          
165600     .                                                                    
165700     EJECT                                                                
165800 HG-AENDRA-LAGG-TILL-TID SECTION.                                         
166000                                                                          
166100     MOVE JA TO AENDRING-SW                                               
166200     MOVE +1  TO X-INDX                                                   
166300     PERFORM UNTIL XXKI-4452-KVPTSORT-X(X-INDX) = MAX-VARDE               
166400       ADD +1 TO X-INDX                                                   
166500     END-PERFORM                                                          
166510                                                                          
166600     ADD +1 TO X-INDX                                                     
166700     PERFORM UNTIL X-INDX > 9                                             
166800       IF MID-KVPTSORT(X-INDX) NOT = ALL '+'                              
166900         MOVE NEJ TO AENDRING-SW                                          
167000         MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-X-ATTR(X-INDX)            
167100       END-IF                                                             
167200       ADD +1 TO X-INDX                                                   
167300     END-PERFORM                                                          
167400                                                                          
167500     SET MID-NUM-IX TO +1                                                 
167600     MOVE +1  TO X-INDX                                                   
167700     MOVE +1  TO 4452-KVPTSORT-X-IX                                       
167800     PERFORM UNTIL MID-KVPTSORT(X-INDX) NOT = ALL '+' OR                  
167900                                X-INDX > 9                                
168000       IF XXKI-4452-KVPTSORT-X(X-INDX) = MAX-VARDE                        
168100           MOVE NEJ TO AENDRING-SW                                        
168200       END-IF                                                             
168300       ADD +1 TO X-INDX                                                   
168400     END-PERFORM                                                          
168500                                                                          
168600     PERFORM UNTIL X-INDX > 10                                            
168800       IF MID-KVPTSORT(X-INDX) NOT = ALL '+'                              
168900         PERFORM HGA-AENDRA-TID                                           
169000       END-IF                                                             
169100       ADD +1 TO X-INDX                                                   
169200     END-PERFORM                                                          
169700     .                                                                    
169800     SKIP3                                                                
170100 HGA-AENDRA-TID SECTION.                                                  
170200                                                                          
170400     IF AENDRING-SW = NEJ AND X-INDX < 10                                 
170600       PERFORM UNTIL X-INDX > 9                                           
170900         IF MID-KVPTSORT(X-INDX) NOT = ALL '+'                            
171000          MOVE MFS-FORMATETS-ATTR TO                                      
171100              MOD-KVPTSORT-X-ATTR(X-INDX)                                 
171200          MOVE MFS-NUM-FAELT-FEL  TO                                      
171300              MOD-KVPTSORT-ATTR(X-INDX)                                   
171400          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPTSORT-IN-ATTR              
171500          ADD +1 TO X-INDX                                                
171600          MOVE 'F' TO AENDRING-SW                                         
171700          MOVE 'W' TO INDATA-SW                                           
171800         END-IF                                                           
171900         ADD +1 TO X-INDX                                                 
172000       END-PERFORM                                                        
172100     END-IF                                                               
172200                                                                          
172500     IF AENDRING-SW = JA                                                  
172700       PERFORM UNTIL MID-KVPTSORT(X-INDX) = ALL '+' OR                    
172800           XXKI-4452-KVPTSORT-X(4452-KVPTSORT-X-IX) = MAX-VARDE           
172900           OR X-INDX > 10                                                 
177200           MOVE ZERO TO XXKI-4452-KVPTID(SPAR-INDX, X-INDX)               
177300           MOVE HELP-MID-AREA(X-INDX) TO                                  
177400                      XXKI-4452-KVPTID(SPAR-INDX, X-INDX)                 
177500           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
177600                      MOD-KVPTID-ATTR(MOD-INDX, X-INDX)                   
177700                                                                          
177900           SET MID-NUM-IX UP BY +1                                        
178000           ADD +1 TO X-INDX                                               
178100           COMPUTE 4452-KVPTSORT-X-IX = X-INDX - 1                        
178200       END-PERFORM                                                        
178400     END-IF                                                               
178800     .                                                                    
178900     EJECT                                                                
179000 HH-TA-BORT-RAD SECTION.                                                  
179100                                                                          
179200     SET XXKI-4452-Y-IX TO +1                                             
179300     SET XXKI-4452-X-IX TO +1                                             
179400     SET HELP-Y-IX TO +1                                                  
179500     SET HELP-X-IX TO +1                                                  
179600     MOVE +1 TO X-INDX                                                    
179700                                                                          
179800     PERFORM UNTIL XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) =                 
179900       KVPTSORT-IN-NUM OR XXKI-4452-Y-IX > 9                              
180000                                                                          
180100       SET XXKI-4452-Y-IX UP BY +1                                        
180200       SET HELP-Y-IX UP BY +1                                             
180300                                                                          
180400     END-PERFORM                                                          
180500                                                                          
180600     IF XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) =                            
180700           KVPTSORT-IN-NUM AND                                            
180800           XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) NOT = MAX-VARDE           
180900                                                                          
181000       MOVE ZERO TO XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX)                  
181100       PERFORM 10 TIMES                                                   
181200         MOVE ZERO TO                                                     
181300            XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX)              
181400         SET XXKI-4452-X-IX UP BY +1                                      
181500       END-PERFORM                                                        
181600                                                                          
181700       PERFORM UNTIL XXKI-4452-Y-IX > 9                                   
181800         SET XXKI-4452-X-IX TO +1                                         
181900         SET HELP-X-IX TO +1                                              
182000         MOVE XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX + 1) TO                 
182100              XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX)                        
182200         MOVE ZERO TO XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX + 1)            
182210                                                                          
182300         PERFORM 10 TIMES                                                 
182400            MOVE                                                          
182410             XXKI-4452-KVPTID(XXKI-4452-Y-IX + 1, XXKI-4452-X-IX)         
182500            TO                                                            
182510             XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX)             
182600            MOVE ZERO TO                                                  
182700            XXKI-4452-KVPTID(XXKI-4452-Y-IX + 1, XXKI-4452-X-IX)          
182800            SET XXKI-4452-X-IX UP BY +1                                   
182900         END-PERFORM                                                      
183000         SET XXKI-4452-Y-IX UP BY +1                                      
183100       END-PERFORM                                                        
183200       PERFORM IMS-REPL-XXKI                                              
183300       PERFORM MFS-RENSA-FAELT-UT                                         
183400       PERFORM MFS-RENSA-FAELT-IN                                         
183500       PERFORM S01-LAES-VISA-INFO                                         
183600                                                                          
183700       MOVE '752' TO MED-IDMFSINF                                         
183800*********************   RADEN ANNULERAD *************************         
183900       CALL WMEDKONV USING MED-WMEDAREA                                   
184000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
184100     ELSE                                                                 
184200       MOVE '014' TO MED-IDMFSFEL                                         
184300**********FEL RADNUMMER **********************************                
184400       CALL WMEDKONV USING MED-WMEDAREA                                   
184500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
184600       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                               
184700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
184800       MOVE MFS-NUM-FAELT-FEL TO MOD-KVPTSORT-IN-ATTR                     
184900       MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT-IN                          
185000     END-IF                                                               
185100     .                                                                    
185200     EJECT                                                                
185300 S01-LAES-VISA-INFO SECTION.                                              
185500                                                                          
185600     PERFORM S02-LAES-GRUNDDATA                                           
185700     IF SEGMENT-FINNS                                                     
185800       MOVE +10 TO Y-INDX                                                 
185900       MOVE +1  TO Z-INDX                                                 
186000       SET  XXKI-4452-Y-IX TO +1                                          
186100       SET  XXKI-4452-X-IX TO +1                                          
186200       MOVE XXKI-4452-KDSORT-Y TO MOD-KDSORT-Y                            
186300       MOVE XXKI-4452-KDSORT-X TO MOD-KDSORT-X                            
186400                                                                          
186500       PERFORM VARYING XXKI-4452-Y-IX FROM 1 BY 1                         
186600          UNTIL XXKI-4452-Y-IX > 10                                       
186700         IF  XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) = ZERO                  
186800           MOVE MFS-RENSA-FAELT TO MOD-KVPTSORT-Y(Y-INDX)                 
186900         ELSE                                                             
187000           MOVE XXKI-4452-KVPTSORT-Y(XXKI-4452-Y-IX) TO                   
187100                     MOD-KVPTSORT-Y(Y-INDX)                               
187200         END-IF                                                           
187300         SUBTRACT 1 FROM Y-INDX                                           
187400       END-PERFORM                                                        
187500                                                                          
187600       MOVE +1 TO Y-INDX                                                  
187700                                                                          
187800       PERFORM VARYING XXKI-4452-Y-IX FROM 1 BY 1                         
187900          UNTIL XXKI-4452-Y-IX > 10                                       
188000         IF  XXKI-4452-KVPTSORT-X(XXKI-4452-Y-IX) = ZERO                  
188100           MOVE MFS-RENSA-FAELT TO MOD-KVPTSORT-X(Y-INDX)                 
188200         ELSE                                                             
188300           MOVE XXKI-4452-KVPTSORT-X(XXKI-4452-Y-IX) TO                   
188400                     MOD-KVPTSORT-X(Y-INDX)                               
188500         END-IF                                                           
188600         ADD +1 TO Y-INDX                                                 
188700       END-PERFORM                                                        
188800                                                                          
188900       MOVE +10 TO Y-INDX                                                 
189000                                                                          
189100       PERFORM VARYING XXKI-4452-Y-IX FROM 1 BY 1                         
189200          UNTIL XXKI-4452-Y-IX > 10                                       
189300                                                                          
189400         MOVE +1 TO X-INDX                                                
189500                                                                          
189600         PERFORM VARYING XXKI-4452-X-IX FROM 1 BY 1                       
189700            UNTIL XXKI-4452-X-IX > 10                                     
189800                                                                          
189900           IF XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX) =          
190000                                               ZERO                       
190100             MOVE MFS-RENSA-FAELT TO                                      
190200                            MOD-KVPTID(Y-INDX, X-INDX)                    
190300           ELSE                                                           
190400                                                                          
190500             MOVE XXKI-4452-KVPTID(XXKI-4452-Y-IX, XXKI-4452-X-IX)        
190600             TO MOD-KVPTID(Y-INDX, X-INDX)                                
190700           END-IF                                                         
190800                                                                          
190900           ADD +1 TO X-INDX                                               
191000         END-PERFORM                                                      
191100       SUBTRACT 1 FROM Y-INDX                                             
191200       ADD +1 TO Z-INDX                                                   
191300       END-PERFORM                                                        
192510     ELSE                                                                 
192600        MOVE '023' TO MED-IDMFSFEL                                        
192700******* TABELL SAKNAS ****************************                        
192800        CALL WMEDKONV USING MED-WMEDAREA                                  
192900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
193000        PERFORM MFS-RENSA-FAELT-UT                                        
193100        PERFORM MFS-RENSA-FAELT-IN                                        
193101     END-IF                                                               
193102                                                                          
193103     .                                                                    
193104     EJECT                                                                
193105                                                                          
193106 S02-LAES-GRUNDDATA SECTION.                                              
193107                                                                          
193108     PERFORM IMS-GET-XXKI-WDGX4451                                        
193109     IF SEGMENT-FINNS                                                     
193110        PERFORM IMS-GET-XXKI-WDGX4452                                     
193111       IF SEGMENT-SAKNAS                                                  
193112          MOVE '023' TO MED-IDMFSFEL                                      
193113********* TABELL SAKNAS ****************************                      
193114          CALL WMEDKONV USING MED-WMEDAREA                                
193115          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
193116          PERFORM MFS-RENSA-FAELT-UT                                      
193117          PERFORM MFS-RENSA-FAELT-IN                                      
193118       END-IF                                                             
193119     ELSE                                                                 
193120        MOVE '023' TO MED-IDMFSFEL                                        
193121******* TABELL SAKNAS ****************************                        
193122        CALL WMEDKONV USING MED-WMEDAREA                                  
193123        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
193124        PERFORM MFS-RENSA-FAELT-UT                                        
193125        PERFORM MFS-RENSA-FAELT-IN                                        
193200     END-IF                                                               
193300     .                                                                    
193400     EJECT                                                                
193500 S03-KOLLA-STORLEK SECTION.                                               
193700                                                                          
193800     MOVE +1 TO 4452-RAD-IX                                               
193900     PERFORM UNTIL XXKI-4452-KVPTSORT-X(4452-RAD-IX) = MAX-VARDE          
194000       OR    4452-RAD-IX > 9                                              
194100       ADD +1 TO 4452-RAD-IX                                              
194200     END-PERFORM                                                          
194300     .                                                                    
194400     EJECT                                                                
194500                                                                          
194600 MFS-RENSA-FAELT-UT SECTION.                                              
194800                                                                          
194900*    --- ALLA UTDATA-FÄLT                                                 
195000     MOVE +1 TO Y-INDX                                                    
195100     MOVE +1 TO X-INDX                                                    
195200     PERFORM UNTIL Y-INDX > SPAR-INDX OR Y-INDX > 10                      
195300       MOVE MFS-RENSA-FAELT TO MOD-KVPTSORT-Y(Y-INDX)                     
195310                                                                          
195400       PERFORM UNTIL X-INDX > SPAR-INDX OR X-INDX > 10                    
195500         MOVE MFS-RENSA-FAELT TO MOD-KVPTID(Y-INDX, X-INDX)               
195600         ADD +1 TO X-INDX                                                 
195700       END-PERFORM                                                        
195800       ADD +1 TO Y-INDX                                                   
195900     END-PERFORM                                                          
196000                                                                          
196100     .                                                                    
196200     SKIP2                                                                
196300 MFS-RENSA-FAELT-IN SECTION.                                              
196500                                                                          
196600*    --- ALLA INDATA-FÄLT                                                 
196700     MOVE +1 TO X-INDX                                                    
196800     MOVE MFS-RENSA-FAELT TO MOD-KVPTSORT-IN                              
196900     PERFORM 10 TIMES                                                     
197000       MOVE MFS-RENSA-FAELT TO MOD-KVPTSORT(X-INDX)                       
197100       ADD +1 TO X-INDX                                                   
197200     END-PERFORM                                                          
197300     MOVE MFS-RENSA-FAELT TO MOD-KDSORT-IN                                
197400     .                                                                    
197500     EJECT                                                                
197600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
197800                                                                          
197900*    --- ALLA UTDATA-FÄLT                                                 
198000     MOVE +1 TO Y-INDX                                                    
198100     MOVE +1 TO X-INDX                                                    
198200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-Y                               
198300     PERFORM 10 TIMES                                                     
198400       MOVE +1 TO X-INDX                                                  
198500       MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT-Y(Y-INDX)                   
198600       MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT-X(Y-INDX)                   
198610                                                                          
198700       PERFORM 10 TIMES                                                   
198800         MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTID(Y-INDX, X-INDX)             
198900         ADD +1 TO X-INDX                                                 
199000       END-PERFORM                                                        
199100       ADD +1 TO Y-INDX                                                   
199200     END-PERFORM                                                          
199300     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-X                               
199400     .                                                                    
199500     SKIP2                                                                
199600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
199800                                                                          
199900*    --- ALLA INDATA-FÄLT                                                 
200000     MOVE +1 TO X-INDX                                                    
200100     MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT-IN                            
200200     PERFORM 10 TIMES                                                     
200300       MOVE MFS-ROER-EJ-FAELT TO MOD-KVPTSORT(X-INDX)                     
200400       ADD +1 TO X-INDX                                                   
200500     END-PERFORM                                                          
200600     MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-IN                              
200700     .                                                                    
200800     EJECT                                                                
200900* --- IMS SEKTIONER ---                                                   
201000*    SKIP3                                                                
201100 IMS-GET-MSG SECTION.                                                     
201300                                                                          
201400     MOVE '  QC' TO GODK-STATUSKODER                                      
201500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
201600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
201700     PERFORM IMS-STATUSKONTROLL                                           
201800     .                                                                    
201900     SKIP3                                                                
202000 IMS-INSERT-MSG SECTION.                                                  
202200                                                                          
202210     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
202400       MOVE '0' TO MFS-KDHUVOMR                                           
202500     END-IF                                                               
202600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
202700     MOVE SPACE TO GODK-STATUSKODER                                       
202800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
202900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
203000     PERFORM IMS-STATUSKONTROLL                                           
203100     .                                                                    
203200     EJECT                                                                
203300 IMS-GET-XXKI-WDGX4451 SECTION.                                           
203500                                                                          
203600     STRING 'WLXXKI01(WDGXKEY  =' W-IDPTIDTAB-X ')'                       
203700          DELIMITED BY SIZE INTO SSA1                                     
203800     MOVE '  GE' TO GODK-STATUSKODER                                      
203900     CALL CBLTDLI USING GU XXKI-PCB DLI-IO-AREA SSA1                      
204000     MOVE XXKI-STATUS-CODE TO STATUS-WS                                   
204100     PERFORM IMS-STATUSKONTROLL                                           
204200     .                                                                    
204300                                                                          
204400                                                                          
204500 IMS-GHU-XXKI-WDGX4451 SECTION.                                           
204700                                                                          
204800     STRING 'WLXXKI01(WDGXKEY  =' W-IDPTIDTAB-X ')'                       
204900          DELIMITED BY SIZE INTO SSA1                                     
205000     MOVE '  GE' TO GODK-STATUSKODER                                      
205100     CALL CBLTDLI USING GHU XXKI-PCB DLI-IO-AREA SSA1                     
205200     MOVE XXKI-STATUS-CODE TO STATUS-WS                                   
205300     PERFORM IMS-STATUSKONTROLL                                           
205400     .                                                                    
205500                                                                          
205600                                                                          
205700 IMS-GET-XXKI-WDGX4452 SECTION.                                           
205900                                                                          
206000     STRING 'WLXXKI11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
206100          DELIMITED BY SIZE INTO SSA1                                     
206200     MOVE '  GE' TO GODK-STATUSKODER                                      
206300     CALL CBLTDLI USING GHNP XXKI-PCB DLI-IO-AREA SSA1                    
206400     MOVE XXKI-STATUS-CODE TO STATUS-WS                                   
206500     PERFORM IMS-STATUSKONTROLL                                           
206600     .                                                                    
206700     EJECT                                                                
206800                                                                          
206900 IMS-ISRT-XXKI-WDGX4451 SECTION.                                          
207100                                                                          
207200     MOVE 'WLXXKI01 ' TO SSA1                                             
207300     MOVE '  II' TO GODK-STATUSKODER                                      
207400     CALL CBLTDLI USING ISRT XXKI-PCB DLI-IO-AREA SSA1                    
207500     MOVE XXKI-STATUS-CODE TO STATUS-WS                                   
207600     PERFORM IMS-STATUSKONTROLL                                           
207700     .                                                                    
207800                                                                          
207900                                                                          
208000 IMS-ISRT-XXKI-WDGX4452 SECTION.                                          
208200                                                                          
208300     STRING 'WLXXKI01(WDGXKEY  =' W-IDPTIDTAB-X ')'                       
208400          DELIMITED BY SIZE INTO SSA1                                     
208500     MOVE 'WLXXKI11 ' TO SSA2                                             
208600     MOVE '  II' TO GODK-STATUSKODER                                      
208700     CALL CBLTDLI USING ISRT XXKI-PCB DLI-IO-AREA SSA1 SSA2               
208800     MOVE XXKI-STATUS-CODE TO STATUS-WS                                   
208900     PERFORM IMS-STATUSKONTROLL                                           
209000     .                                                                    
209100     EJECT                                                                
209200                                                                          
209300 IMS-REPL-XXKI SECTION.                                                   
209500                                                                          
209600     MOVE '  ' TO GODK-STATUSKODER                                        
209700     CALL CBLTDLI USING REPL XXKI-PCB DLI-IO-AREA                         
209800     MOVE XXKI-STATUS-CODE TO STATUS-WS                                   
209900     PERFORM IMS-STATUSKONTROLL                                           
210000     .                                                                    
210100                                                                          
210200                                                                          
210300 IMS-DLET-XXKI-WDGX4452 SECTION.                                          
210500                                                                          
210600     MOVE '  ' TO GODK-STATUSKODER                                        
210700     CALL CBLTDLI USING DLET XXKI-PCB DLI-IO-AREA                         
210800     MOVE XXKI-STATUS-CODE TO STATUS-WS                                   
210900     PERFORM IMS-STATUSKONTROLL                                           
211000     .                                                                    
211100                                                                          
211110 IMS-GU-WDB601    SECTION.                                                
211120     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
211130          DELIMITED BY SIZE INTO SSA1                                     
211140     MOVE '  GE' TO GODK-STATUSKODER                                      
211150     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
211160     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
211170     PERFORM IMS-STATUSKONTROLL                                           
211180     IF SEGMENT-SAKNAS                                                    
211190         MOVE SPACE TO DCS-KDDC                                           
211191     END-IF                                                               
211192     .                                                                    
211200                                                                          
211300 IMS-STATUSKONTROLL SECTION.                                              
211400                                                                          
211500     SET STATUS-IX TO 1                                                   
211600     SEARCH GODK-STATUS                                                   
211700       AT END CALL FELLOG                                                 
211800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
211900     END-SEARCH                                                           
212000     .                                                                    
