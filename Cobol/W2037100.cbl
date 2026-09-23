000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2037100.                                                
000300 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000400 DATE-WRITTEN.   96/06/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        ÖVERSIKTSBILD SOM VISAR HUR MÅNGA ORDERFÖRSLAG                   
000900*        PER BUYER SOM ÅTERSTÅR ATT BEDÖMMA .                             
001000*        GENOM ATT ANGE ETT 'S' FRAMFÖR BUYER OCH TRYCKA PF14             
001100*        KOMMER MAN ÖVER TILL 2372 OCH FÅR UPP FÖRSTA ICKE                
001200*        BEDÖMDA ORDERFÖRSLAG FÖR ANGIVEN BUYER                           
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLORDL (WDE3)                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T371                                              
001800*        MID:         W2I37101                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W2O37101                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W2037100'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000*    FÖR ATT GARANTERA PLATS FÖR TOTALEN                                  
004100*    SÄTTS MAX-INDX TILL 11                                               
004200 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004300*    TOTALT ANTALET RADER PÅ BILDEN ÄR 13                                 
004400 77  MAX-INDX-SIDA               PIC S9(4)  VALUE +13   COMP SYNC.        
004500                                                                          
004600 01  WS.                                                                  
004700                                                                          
004800*********************************************************                 
004900*    WS-MSGI-AREA-2371                                                    
005000*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
005100*           (I MSGI-SPAR-AREA)                                            
005200*********************************************************                 
005300  05 WS-MSGI-AREA-2371.                                                   
005400    10 WS-MSGI-IDTRANS-2371      PIC X(4)    VALUE '2371'.                
005500    10 WS-MSGI-SSA-KEY-ENTER     PIC X(14)   VALUE SPACE.                 
005600    10 WS-MSGI-SSA-KEY-NEXT      PIC X(14)   VALUE SPACE.                 
005700    10 FILLER                    PIC X(168)  VALUE SPACE.                 
005800                                                                          
005900*********************************************************                 
006000*    WS-MSGI-AREA-2372                                                    
006100*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
006200*           (I MSGI-SPAR-AREA)                                            
006300*           WS-MSGI-IDTYPE ANVÄNDS FÖR ATT FÅ UPP RÄTT KÖ                 
006400*********************************************************                 
006500  05 WS-MSGI-AREA-2372.                                                   
006600    10 WS-MSGI-IDTRANS-2372      PIC X(4)    VALUE '2372'.                
006700    10 WS-MSGI-IDARTNR-ENTER     PIC  9(9)   VALUE ZERO.                  
006800    10 WS-MSGI-ORDER-ENTER       PIC X       VALUE SPACE.                 
006900    10 WS-MSGI-IDARTNR-PF7       PIC  9(9)   VALUE ZERO.                  
007000    10 WS-MSGI-ORDER-PF7         PIC X       VALUE SPACE.                 
007100    10 WS-MSGI-IDTYPE            PIC X       VALUE SPACE.                 
007200                                                                          
007300  05 WS-WDE301KY-ENTER.                                                   
007400    10 WS-IDDC-ENTER             PIC X(2)    VALUE SPACE.                 
007500    10 WS-IDPERSON-BUY-ENTER     PIC S9(3)   VALUE ZERO COMP-3.           
007600    10 WS-KDREFTYP-ENTER         PIC X       VALUE SPACE.                 
007700    10 WS-IDARTNR-ENTER          PIC S9(9)   VALUE ZERO COMP-3.           
007800    10 WS-IDDISTR-ENTER          PIC S9(5)   VALUE ZERO COMP-3.           
007900  05 WS-WDE301KY-NEXT.                                                    
008000    10 WS-IDDC-NEXT              PIC X(2)    VALUE SPACE.                 
008100    10 WS-IDPERSON-BUY-NEXT      PIC S9(3)   VALUE ZERO COMP-3.           
008200    10 WS-KDREFTYP-NEXT          PIC X       VALUE SPACE.                 
008300    10 WS-IDARTNR-NEXT           PIC S9(9)   VALUE ZERO COMP-3.           
008400    10 WS-IDDISTR-NEXT           PIC S9(5)   VALUE ZERO COMP-3.           
008500                                                                          
008600  05 WS-IDTYPE                   PIC X       VALUE SPACE.                 
008700  05 WS-BUY-TO-REVIEW            PIC S9(6)   VALUE ZERO.                  
008800  05 WS-TOT-TO-REVIEW            PIC S9(6)   VALUE ZERO.                  
008900  05 WS-RED-ANTAL                PIC Z(5)9.                               
009000  05 WS-SPARA-IDPERSON-BUY       PIC S9(3)   VALUE ZERO COMP-3.           
009100  05 WS-SPARA-KDREFTYP           PIC X       VALUE SPACE.                 
009200  05 WS-SPARA-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
009300                                                                          
009400*      --- VALID IDDC CODES                                               
009500*                                                                         
009600*01    -COPY WWDC99                                                       
009700       EJECT                                                              
009800                                                                          
009900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010000                                                                          
010100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010200     88  NYCKLAR-OK                          VALUE 'J'.                   
010300     88  NYCKLAR-FEL                         VALUE 'N'.                   
010400                                                                          
010500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010600     88  EGEN-MID                            VALUE '2371'.                
010700     88  GODK-MID                            VALUE '2351' '2352'          
010800                                                   '2353' '2354'          
010900                                                   '2355' '2356'          
011000                                                   '2357' '2358'          
011100                                                   '2359'.                
011200     88  HELP-MID                            VALUE '0551'.                
011300     EJECT                                                                
011400                                                                          
011500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011600 01  GENERELLA-SUBPROGRAM.                                                
011700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012200     EJECT                                                                
012300                                                                          
012400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012500*01 -COPY WMEDAREA                                                        
012600     SKIP3                                                                
012700 01  MESSAGE-CODES.                                                       
012800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013000     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
013100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013300     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
013400     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
013500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013700     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
013800     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
013900     03  ARTIKEL-EJ-AKTIV        PIC X(3)    VALUE '244'.                 
014000     03  RAD-FINNS               PIC X(3)    VALUE '245'.                 
014100     03  QUEUED-PRINTER          PIC X(3)    VALUE '246'.                 
014200     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
014300     03  MARKERA-RAD             PIC X(3)    VALUE '309'.                 
014400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014500     03  INF-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
014600                                                                          
014700 01  MEDDELANDE.                                                          
014800     03  MED-1                   PIC X(30)                                
014900         VALUE 'TYPE : A,B,C OR L             '.                          
015000     03  MED-2                   PIC X(30)                                
015100         VALUE 'TO CHANGE SCREEN; PRESS PF14  '.                          
015200     EJECT                                                                
015300                                                                          
015400*01  -COPY WDATAREA                                                       
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015900     SKIP3                                                                
016000*01 -COPY WMSGINIT                                                        
016100     SKIP3                                                                
016200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016300*                                                                         
016400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016500     SKIP3                                                                
016600*01  MID -COPY W2I37101                                                   
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016900     SKIP3                                                                
017000*01  -COPY WMSGAREA                                                       
017100     EJECT                                                                
017200     03  MOD REDEFINES MSG-AREA.                                          
017300*      05  -COPY W2O37101                                                 
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017600     SKIP3                                                                
017700*01  -COPY WMFSAREA                                                       
017800     EJECT                                                                
017900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018000*                                                                         
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018300     SKIP3                                                                
018400 01  NYCKLAR-TILL-DLI.                                                    
018500                                                                          
018600     03 W-WDE301KY-X.                                                     
018700         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
018800         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
018900         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
019000         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
019100         05  W-IDDISTR           PIC S9(7) VALUE ZERO COMP-3.             
019200                                                                          
019300     03 W-WDE301KY-MIN-X.                                                 
019400         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
019500         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
019600         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
019700         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
019800         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
019900                                                                          
020000     03 W-WDE301KY-MAX-X.                                                 
020100         05  W-IDDC-MAX          PIC X(2)  VALUE SPACE.                   
020200         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE +999 COMP-3.             
020300         05  W-KDREFTYP-MAX      PIC X     VALUE HIGH-VALUE.              
020400         05  W-IDARTNR-MAX       PIC S9(9)                                
020500                                         VALUE +999999999 COMP-3.         
020600         05  W-IDDISTR-MAX       PIC S9(5) VALUE +99999 COMP-3.           
020700                                                                          
020800     03  W-IDDC-B6-X.                                                     
020900         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
021000                                                                          
021100     SKIP2                                                                
021200                                                                          
021300*    --- STATUS-KOD FRÅN IMS                                              
021400 01  STATUS-WS                   PIC XX.                                  
021500     88  SEGMENT-FINNS                       VALUE '  '.                  
021600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021700     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
021800                                                   'GB'.                  
021900     SKIP2                                                                
022000 01  GODK-STATUSKODER.                                                    
022100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022200     SKIP3                                                                
022300 01  SSA1                        PIC X(64).                               
022400 01  SSA2                        PIC X(64).                               
022500     EJECT                                                                
022600                                                                          
022700*    --- IMS FUNKTIONSKODER                                               
022800*01  -COPY W0003                                                          
022900     EJECT                                                                
023000                                                                          
023100*    ---  DLI INPUT-OUTPUT AREA                                           
023200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
023300     SKIP3                                                                
023400 01  DLI-IO-AREA.                                                         
023500     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
023600     SKIP3                                                                
023700     03  WLORDL01 REDEFINES IO-AREA.                                      
023800*        05  -COPY WDE301                                                 
023900     EJECT                                                                
024000                                                                          
024100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024200 01   DLI-IO-AREA-B601.                                                   
024300*     03  -COPY WDB601                                                    
024400     EJECT                                                                
024500                                                                          
024600 LINKAGE SECTION.                                                         
024700*01  -COPY W0009   -PRE MSG-                                              
024800*01  -COPY W0008   -PRE USEA-                                             
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025100*01  -COPY W0008  -PRE ORDL-                                              
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400*01  -COPY W0008      -PRE WDB6-                                          
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700                                                                          
025800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ORDL-PCB                      
025900                           WDB6-PCB.                                      
026000 MAIN SECTION.                                                            
026100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORDL-PCB                      
026200                           WDB6-PCB.                                      
026300                                                                          
026400     PERFORM IMS-GET-MSG                                                  
026500     IF SEGMENT-FINNS                                                     
026600       PERFORM A-INIT                                                     
026700       PERFORM B-KOLLA-NYCKLAR                                            
026800       IF NYCKLAR-OK                                                      
026900         IF MFS-FIRST                                                     
027000           PERFORM C-FOERSTA-SIDA                                         
027100         ELSE                                                             
027200           IF MFS-NEXT                                                    
027300             PERFORM D-NAESTA-SIDA                                        
027400           ELSE                                                           
027500             PERFORM E-SAMMA-SIDA                                         
027600           END-IF                                                         
027700         END-IF                                                           
027800         PERFORM F-LAES-VISA-INFO                                         
027900                                                                          
028000         MOVE WS-MSGI-AREA-2371 TO MSGI-SPAR-AREA                         
028100         MOVE '002'             TO MSGI-KDCALL                            
028200         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
028300         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
028400         MOVE '2371'            TO MSGI-IDTRANS                           
028500         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
028600                                                                          
028700       END-IF                                                             
028800       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O37101 + 4                      
028900       PERFORM IMS-INSERT-MSG                                             
029000     END-IF                                                               
029100                                                                          
029200     MOVE ZERO TO RETURN-CODE                                             
029300     GOBACK                                                               
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700 A-INIT SECTION.                                                          
029800                                                                          
029900     IF MSG-DUBBLA-TRANSKODER                                             
030000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I37101                 
030100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
030200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030300     ELSE                                                                 
030400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I37101                  
030500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
030600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030700     END-IF                                                               
030800                                                                          
030900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031200                                                                          
031300     MOVE LOW-VALUE TO MSG-AREA                                           
031400     MOVE 'W2O371N1' TO MFS-IDMOD                                         
031500     MOVE '2371' TO MOD-IDTRANS                                           
031600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031700                                                                          
031800     IF EGEN-MID OR HELP-MID                                              
031900       CONTINUE                                                           
032000     ELSE                                                                 
032100       MOVE 'A'              TO MID-IDTYPE-2371-IN                        
032200       MOVE SPACE TO MFS-KDTRTYP                                          
032300       MOVE '7' TO MFS-IDPFK                                              
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700                                                                          
032800 B-KOLLA-NYCKLAR SECTION.                                                 
032900                                                                          
033000******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
033100******   I SLUTET AV PROGRAMMET                                           
033200     MOVE ALL '+'               TO MSGI-WMSGINIT                          
033300     MOVE '001'                 TO MSGI-KDCALL                            
033400     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
033500     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
033600     MOVE '2371'                TO MSGI-IDTRANS                           
033700     IF EGEN-MID                                                          
033800       MOVE MID-IDDC-2371-IN TO MSGI-IDDC-KEY                             
033900     END-IF                                                               
034000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034100                                                                          
034200     IF W-IDTRANS            = '2372'                                     
034300     AND MSGI-SPAR-AREA(1:4) = '2372'                                     
034400*                                                                         
034500*      -- FÖR ATT HÄMTA IDTYPE                                            
034600*                                                                         
034700       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-2372                         
034800       MOVE WS-MSGI-IDTYPE   TO WS-IDTYPE                                 
034900                                                                          
035000     ELSE                                                                 
035100                                                                          
035200       IF EGEN-MID                                                        
035300         IF MSGI-SPAR-AREA(1:4) = '2371'                                  
035400           MOVE MSGI-SPAR-AREA                                            
035500                            TO WS-MSGI-AREA-2371                          
035600         END-IF                                                           
035700       END-IF                                                             
035800                                                                          
035900       IF MID-IDTYPE-2371-IN = ALL '+'                                    
036000         IF MID-IDTYPE-2371-UT = 'BOAT '                                  
036100           MOVE 'B'          TO WS-IDTYPE                                 
036200         END-IF                                                           
036300         IF MID-IDTYPE-2371-UT = 'AIR  '                                  
036400           MOVE 'A'          TO WS-IDTYPE                                 
036500         END-IF                                                           
036600         IF MID-IDTYPE-2371-UT = 'AIRCR'                                  
036700           MOVE 'C'          TO WS-IDTYPE                                 
036800         END-IF                                                           
036900         IF MID-IDTYPE-2371-UT = 'LOCAL'                                  
037000           MOVE 'L'          TO WS-IDTYPE                                 
037100         END-IF                                                           
037200       ELSE                                                               
037300         MOVE MID-IDTYPE-2371-IN TO WS-IDTYPE                             
037400       END-IF                                                             
037500     END-IF                                                               
037600                                                                          
037700     MOVE JA TO NYCKLAR-SW                                                
037800*    -- KONTROLL AV IDDC                                                  
037900     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
038000                                                                          
038100     MOVE MSGI-IDDC-KEY      TO WS-IDDC                                   
038200                                W-IDDC-301                                
038300                                W-IDDC-MIN                                
038400                                W-IDDC-MAX                                
038500                                W-IDDC-B6                                 
038600     PERFORM IMS-GU-WDB601                                                
038700     IF  SEGMENT-FINNS                                                    
038800     AND (DCS-SDC                                                         
038900     OR   DCS-NDC-PF                                                      
039000     OR   DCS-NDC-CN                                                      
039001     OR   DCS-NDC-SA                                                      
039010     OR   DCS-NDC-OTHERS)                                                 
039100         CONTINUE                                                         
039200     ELSE                                                                 
039300       MOVE NEJ TO NYCKLAR-SW                                             
039400     END-IF                                                               
039500     MOVE WS-IDDC            TO MOD-IDDC-UT                               
039600                                                                          
039700*      -- KONTROLL AV TYP                                                 
039800     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-IN                             
039900                                                                          
040000     IF    WS-IDTYPE = 'A'                                                
040100     OR    WS-IDTYPE = 'C'                                                
040200     OR    WS-IDTYPE = 'B'                                                
040300     OR    WS-IDTYPE = 'L'                                                
040400       IF WS-IDTYPE = 'A'                                                 
040500         MOVE 'AIR'          TO MOD-IDTYPE-UT                             
040600       END-IF                                                             
040700       IF WS-IDTYPE = 'C'                                                 
040800         MOVE 'AIRCR'        TO MOD-IDTYPE-UT                             
040900       END-IF                                                             
041000       IF WS-IDTYPE = 'B'                                                 
041100         MOVE 'BOAT'         TO MOD-IDTYPE-UT                             
041200       END-IF                                                             
041300       IF WS-IDTYPE = 'L'                                                 
041400         MOVE 'LOCAL'        TO MOD-IDTYPE-UT                             
041500       END-IF                                                             
041600     ELSE                                                                 
041700       MOVE NEJ TO NYCKLAR-SW                                             
041800       MOVE MED-1            TO MOD-TEMFSINF                              
041900     END-IF                                                               
042000                                                                          
042100                                                                          
042200     IF NYCKLAR-FEL                                                       
042300       MOVE 'GB '         TO MED-IDSKYLT                                  
042400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
042500       CALL WMEDKONV USING MED-WMEDAREA                                   
042600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
042700       PERFORM MFS-RENSA-FAELT-IN                                         
042800       PERFORM MFS-RENSA-FAELT-UT                                         
042900     END-IF                                                               
043000     .                                                                    
043100     EJECT                                                                
043200                                                                          
043300 C-FOERSTA-SIDA SECTION.                                                  
043400                                                                          
043500     MOVE 'GB '           TO MED-IDSKYLT                                  
043600     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
043700     CALL WMEDKONV USING MED-WMEDAREA                                     
043800     MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                  
043900                                                                          
044000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
044100     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
044200                                WS-MSGI-SSA-KEY-NEXT                      
044300     PERFORM MFS-RENSA-FAELT-IN                                           
044400     .                                                                    
044500     EJECT                                                                
044600                                                                          
044700 D-NAESTA-SIDA SECTION.                                                   
044800                                                                          
044900     IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                                  
045000       MOVE WS-MSGI-SSA-KEY-NEXT                                          
045100                             TO W-WDE301KY-MIN-X                          
045200     END-IF                                                               
045300     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
045400                                WS-MSGI-SSA-KEY-NEXT                      
045500     .                                                                    
045600     EJECT                                                                
045700                                                                          
045800 E-SAMMA-SIDA SECTION.                                                    
045900                                                                          
046000     IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                                 
046100       MOVE WS-MSGI-SSA-KEY-ENTER                                         
046200                             TO W-WDE301KY-MIN-X                          
046300     END-IF                                                               
046400     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
046500                                WS-MSGI-SSA-KEY-NEXT                      
046600     MOVE MED-2              TO MOD-TEMFSINF                              
046700     .                                                                    
046800     EJECT                                                                
046900                                                                          
047000 F-LAES-VISA-INFO SECTION.                                                
047100                                                                          
047200     MOVE +1                 TO INDX                                      
047300     PERFORM IMS-GU-WDE3-ORDL01-MIN-MAX                                   
047400     IF SEGMENT-FINNS                                                     
047500       MOVE REF-IDDC         TO WS-IDDC-ENTER                             
047600       MOVE REF-IDPERSON-BUY TO WS-IDPERSON-BUY-ENTER                     
047700       MOVE REF-KDREFTYP     TO WS-KDREFTYP-ENTER                         
047800       MOVE REF-IDARTNR      TO WS-IDARTNR-ENTER                          
047900       MOVE REF-IDDISTR      TO WS-IDDISTR-ENTER                          
048000       MOVE WS-WDE301KY-ENTER                                             
048100                             TO WS-MSGI-SSA-KEY-ENTER                     
048200                                                                          
048300       PERFORM UNTIL INDX    > MAX-INDX                                   
048400       OR SEGMENT-SAKNAS                                                  
048500           MOVE REF-IDPERSON-BUY                                          
048600                             TO WS-SPARA-IDPERSON-BUY                     
048700           MOVE REF-KDREFTYP TO WS-SPARA-KDREFTYP                         
048800           MOVE ZERO         TO WS-BUY-TO-REVIEW                          
048900           PERFORM UNTIL SEGMENT-SAKNAS                                   
049000           OR NOT (REF-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY               
049100           AND REF-KDREFTYP = WS-SPARA-KDREFTYP)                          
049200             IF REF-KDREFORS = 'P'                                        
049300             AND WS-IDTYPE = REF-KDREFTYP                                 
049400             AND WS-IDDC   = REF-IDDC                                     
049500               ADD +1        TO WS-BUY-TO-REVIEW                          
049600                                WS-TOT-TO-REVIEW                          
049700               MOVE REF-IDARTNR                                           
049800                             TO WS-SPARA-IDARTNR                          
049900               PERFORM UNTIL SEGMENT-SAKNAS                               
050000               OR NOT (REF-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY           
050100               AND REF-KDREFTYP = WS-SPARA-KDREFTYP                       
050200               AND REF-IDARTNR = WS-SPARA-IDARTNR)                        
050300                 PERFORM IMS-GN-WDE3-ORDL01-MIN-MAX                       
050400               END-PERFORM                                                
050500             ELSE                                                         
050600               PERFORM IMS-GN-WDE3-ORDL01-MIN-MAX                         
050700             END-IF                                                       
050800                                                                          
050900           END-PERFORM                                                    
051000                                                                          
051100           IF WS-BUY-TO-REVIEW > ZERO                                     
051200             MOVE WS-SPARA-IDPERSON-BUY                                   
051300                             TO MOD-IDPERSON-BUY (INDX)                   
051400             MOVE MFS-ADD-LAES-IN-FAELT                                   
051500                             TO MOD-IDPERSON-BUY-ATTR (INDX)              
051600             MOVE WS-BUY-TO-REVIEW                                        
051700                             TO WS-RED-ANTAL                              
051800             MOVE WS-RED-ANTAL                                            
051900                             TO MOD-TO-REVIEW (INDX)                      
052000             ADD 1           TO INDX                                      
052100           END-IF                                                         
052200       END-PERFORM                                                        
052300                                                                          
052400       IF SEGMENT-FINNS                                                   
052500         MOVE REF-IDDC       TO WS-IDDC-NEXT                              
052600         MOVE REF-IDPERSON-BUY                                            
052700                             TO WS-IDPERSON-BUY-NEXT                      
052800         MOVE REF-KDREFTYP   TO WS-KDREFTYP-NEXT                          
052900         MOVE REF-IDARTNR    TO WS-IDARTNR-NEXT                           
053000         MOVE REF-IDDISTR    TO WS-IDDISTR-NEXT                           
053100         MOVE WS-WDE301KY-NEXT                                            
053200                             TO WS-MSGI-SSA-KEY-NEXT                      
053300         MOVE 'GB '          TO MED-IDSKYLT                               
053400         MOVE INF-MORE-INFO-EXISTS                                        
053500                             TO MED-IDMFSFEL                              
053600         CALL WMEDKONV USING MED-WMEDAREA                                 
053700         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
053800       ELSE                                                               
053900         MOVE SPACE          TO WS-KDREFTYP-NEXT                          
054000                                WS-IDDC-NEXT                              
054100         MOVE ZERO           TO WS-IDPERSON-BUY-NEXT                      
054200                                WS-IDARTNR-NEXT                           
054300                                WS-IDDISTR-NEXT                           
054400         MOVE ZERO           TO W-IDPERSON-BUY-MIN                        
054500                                W-IDARTNR-MIN                             
054600         MOVE SPACE          TO WS-MSGI-SSA-KEY-NEXT                      
054700         MOVE ZERO           TO WS-TOT-TO-REVIEW                          
054800         PERFORM IMS-GU-WDE3-ORDL01-MIN-MAX                               
054900                                                                          
055000         PERFORM UNTIL SEGMENT-SAKNAS                                     
055100           IF REF-KDREFORS = 'P'                                          
055200           AND WS-IDTYPE   = REF-KDREFTYP                                 
055300           AND WS-IDDC     = REF-IDDC                                     
055400             ADD +1          TO WS-TOT-TO-REVIEW                          
055500             MOVE REF-IDPERSON-BUY                                        
055600                             TO WS-SPARA-IDPERSON-BUY                     
055700             MOVE REF-KDREFTYP                                            
055800                             TO WS-SPARA-KDREFTYP                         
055900             MOVE REF-IDARTNR                                             
056000                             TO WS-SPARA-IDARTNR                          
056100                                                                          
056200             PERFORM UNTIL SEGMENT-SAKNAS                                 
056300             OR NOT (REF-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY             
056400             AND REF-KDREFTYP = WS-SPARA-KDREFTYP                         
056500             AND REF-IDARTNR = WS-SPARA-IDARTNR)                          
056600                                                                          
056700               PERFORM IMS-GN-WDE3-ORDL01-MIN-MAX                         
056800             END-PERFORM                                                  
056900           ELSE                                                           
057000             PERFORM IMS-GN-WDE3-ORDL01-MIN-MAX                           
057100           END-IF                                                         
057200                                                                          
057300         END-PERFORM                                                      
057400         ADD +1              TO INDX                                      
057500         MOVE WS-TOT-TO-REVIEW                                            
057600                             TO WS-RED-ANTAL                              
057700         MOVE WS-RED-ANTAL   TO MOD-TO-REVIEW (INDX)                      
057800       END-IF                                                             
057900     ELSE                                                                 
058000       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
058100       IF MFS-FIRST                                                       
058200         MOVE 'GB '          TO MED-IDSKYLT                               
058300         MOVE URVAL-SAKNAS   TO MED-IDMFSFEL                              
058400         CALL WMEDKONV USING MED-WMEDAREA                                 
058500         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
058600       END-IF                                                             
058700     END-IF                                                               
058800     .                                                                    
058900     EJECT                                                                
059000                                                                          
059100 MFS-RENSA-FAELT-UT SECTION.                                              
059200                                                                          
059300*    --- ALLA UTDATA-FÄLT                                                 
059400     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-UT                             
059500                                                                          
059600     MOVE 1                  TO INDX                                      
059700     PERFORM UNTIL INDX > 13                                              
059800       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
059900       ADD 1                 TO INDX                                      
060000     END-PERFORM                                                          
060100     .                                                                    
060200     SKIP3                                                                
060300                                                                          
060400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
060500                                                                          
060600*    --- ALLA UTDATA-FÄLT                                                 
060700                                                                          
060800     MOVE MFS-RENSA-FAELT    TO MOD-SELECT (INDX)                         
060900                                MOD-IDPERSON-BUY (INDX)                   
061000                                MOD-TO-REVIEW (INDX)                      
061100     .                                                                    
061200     SKIP3                                                                
061300                                                                          
061400 MFS-RENSA-FAELT-IN SECTION.                                              
061500                                                                          
061600*    --- ALLA INDATA-FÄLT                                                 
061700     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-IN                             
061800                                                                          
061900     MOVE 1                  TO INDX                                      
062000     PERFORM UNTIL INDX > 13                                              
062100       MOVE MFS-RENSA-FAELT  TO MOD-SELECT (INDX)                         
062200       ADD 1                 TO INDX                                      
062300     END-PERFORM                                                          
062400     .                                                                    
062500     EJECT                                                                
062600                                                                          
062700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
062800                                                                          
062900*    --- ALLA UTDATA-FÄLT                                                 
063000     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTYPE-UT                             
063100                                MOD-TEMFSINF                              
063200     MOVE +1 TO INDX                                                      
063300     PERFORM UNTIL INDX > MAX-INDX                                        
063400       MOVE MFS-ROER-EJ-FAELT                                             
063500                             TO MOD-SELECT (INDX)                         
063600                                MOD-IDPERSON-BUY (INDX)                   
063700                                MOD-TO-REVIEW (INDX)                      
063800       ADD +1 TO INDX                                                     
063900     END-PERFORM                                                          
064000     .                                                                    
064100     SKIP2                                                                
064200                                                                          
064300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
064400                                                                          
064500*    --- ALLA INDATA-FÄLT                                                 
064600     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTYPE-IN                             
064700     .                                                                    
064800     EJECT                                                                
064900* --- IMS SEKTIONER ---                                                   
065000     SKIP3                                                                
065100                                                                          
065200 IMS-GET-MSG SECTION.                                                     
065300                                                                          
065400     MOVE '  QC' TO GODK-STATUSKODER                                      
065500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
065600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065700     PERFORM IMS-STATUSKONTROLL                                           
065800     .                                                                    
065900     SKIP3                                                                
066000                                                                          
066100 IMS-INSERT-MSG SECTION.                                                  
066200                                                                          
066300     IF ENGLISH-TEXT                                                      
066400       MOVE 'N' TO MFS-KDHUVOMR                                           
066500     END-IF                                                               
066600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
066700     MOVE SPACE TO GODK-STATUSKODER                                       
066800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
066900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067000     PERFORM IMS-STATUSKONTROLL                                           
067100     .                                                                    
067200     EJECT                                                                
067300                                                                          
067400 IMS-GU-WDE3-ORDL01-MIN-MAX SECTION.                                      
067500                                                                          
067600     STRING 'WLORDL01(WDE301KY>=' W-WDE301KY-MIN-X                        
067700                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
067800          DELIMITED BY SIZE INTO SSA1                                     
067900     MOVE '  GE' TO GODK-STATUSKODER                                      
068000     CALL CBLTDLI USING GU ORDL-PCB DLI-IO-AREA SSA1                      
068100     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
068200     PERFORM IMS-STATUSKONTROLL                                           
068300     .                                                                    
068400     SKIP3                                                                
068500                                                                          
068600 IMS-GN-WDE3-ORDL01-MIN-MAX SECTION.                                      
068700                                                                          
068800     STRING 'WLORDL01(WDE301KY>=' W-WDE301KY-MIN-X                        
068900                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
069000          DELIMITED BY SIZE INTO SSA1                                     
069100     MOVE '  GE' TO GODK-STATUSKODER                                      
069200     CALL CBLTDLI USING GN ORDL-PCB DLI-IO-AREA SSA1                      
069300     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
069400     PERFORM IMS-STATUSKONTROLL                                           
069500     .                                                                    
069600     EJECT                                                                
069700                                                                          
069800 IMS-GU-WDB601    SECTION.                                                
069900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
070000          DELIMITED BY SIZE INTO SSA1                                     
070100     MOVE '  GE' TO GODK-STATUSKODER                                      
070200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
070300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
070400     PERFORM IMS-STATUSKONTROLL                                           
070500     .                                                                    
070600     EJECT                                                                
070700                                                                          
070800 IMS-STATUSKONTROLL SECTION.                                              
070900                                                                          
071000     SET STATUS-IX TO 1                                                   
071100     SEARCH GODK-STATUS                                                   
071200       AT END                                                             
071300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
071400         DELIMITED BY SIZE INTO FELTEXT                                   
071500         CALL FELLOG                                                      
071600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
071700         CONTINUE                                                         
071800     END-SEARCH                                                           
071900     .                                                                    
