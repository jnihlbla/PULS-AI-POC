004099 PROCESS DYNAM                                                            
010000 ID DIVISION.                                                             
020000     SKIP2                                                                
030099 PROGRAM-ID.     W2031600.                                                
040099*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
050099*DATE-WRITTEN.   JULI 2003.                                               
060000                                                                          
070000*    REMARKS.                                                             
080000*                                                                         
090000*    FUNKTION:                                                            
100099*        PROGRAMMETS UPPGIFT ÄR ATT GRUPPERA KAMPANJER                    
100199*                                                                         
101099*       EXEMPEL PÅ DB2KOD SE W3011100, WF025200                           
102099*       EXEMPEL PÅ BLÄDDRING SE W4010800                                  
110000*                                                                         
160085*                                                                         
170000*    INDATA.                                                              
180099*        TRANSAKTION: W2T316                                              
190099*        MID:         W2I31601                                            
200000*                                                                         
210000*    UTDATA.                                                              
220099*        MOD:         W2O31601                                            
230000                                                                          
240000     SKIP3                                                                
250000 ENVIRONMENT DIVISION.                                                    
260000     EJECT                                                                
270000 DATA DIVISION.                                                           
280000 WORKING-STORAGE SECTION.                                                 
290099*    -COPY WY2000W1                                                       
300099     SKIP3                                                                
310099 77  IDPGM                       PIC X(08)   VALUE 'W2031600'.            
320000                                                                          
330000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
340099 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
350000                                                                          
360000 77  JA                          PIC X       VALUE 'J'.                   
370099 77  YES                         PIC X       VALUE 'Y'.                   
380000 77  NEJ                         PIC X       VALUE 'N'.                   
390099 77  IX                          PIC 9(9)    VALUE ZERO.                  
391099 77  RAD-IX                      PIC 9(9)    VALUE ZERO.                  
392099 77  RAD-MAX                     PIC 9(9)    VALUE 13.                    
392199 77  IX-SISTA-POST               PIC 9(9)    VALUE ZERO.                  
400099 77  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
410000                                                                          
420006 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
440000                                                                          
450000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
490045                                                                          
500039 77  SPAR-KDERS                  PIC 9(3)    VALUE ZERO.                  
510000                                                                          
520006 77  ARTIKEL-ERS-SW              PIC X       VALUE 'N'.                   
530006     88  ARTIKEL-ERS-MAERKT                  VALUE 'J'.                   
540006                                                                          
541099 77  ARTIKEL-FINNS-I-PULS-SW     PIC X       VALUE 'J'.                   
541199     88  ARTIKEL-FINNS-EJ-I-PULS             VALUE 'N'.                   
543099                                                                          
550000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
560000     88  INDATA-OK                           VALUE 'J'.                   
570000     88  INDATA-FEL                          VALUE 'N'.                   
580000                                                                          
590000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
600000     88  NYCKLAR-OK                          VALUE 'J'.                   
610000     88  NYCKLAR-FEL                         VALUE 'N'.                   
620000                                                                          
630000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
640031     88  HELP-MID                            VALUE '0551'.                
650099     88  EGEN-MID                            VALUE '2316'.                
650100     88  2317-MID                            VALUE '2317'.                
660099     88  GODK-MID                            VALUE '2316' '2342'          
670099                                                   '2343'.                
671099     EJECT                                                                
672099*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
673099 01  TABENTRY-PARM.                                                       
674099     03  STEGLANGD               PIC S9(9) COMP  VALUE 54.                
675099     03  ANTAL                   PIC S9(9) COMP.                          
676099     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
677099                                                                          
680099                                                                          
690099 01  WS.                                                                  
691099*********************************************************                 
692099*    WS-MSGI-AREA-2316                                                    
693099*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
694099*           (I MSGI-SPAR-AREA)                                            
695099*********************************************************                 
696099  05 WS-MSGI-AREA-2316.                                                   
697099    10 WS-MSGI-IDTRANS-2316      PIC X(4)    VALUE '2316'.                
698099    10 WS-MSGI-SSA-KEY-ENTER.                                             
699099      15  WS-ENTER-IDKAMP        PIC X(7)    VALUE SPACE.                 
699199    10 WS-MSGI-SSA-KEY-NEXT.                                              
699299      15  WS-NEXT-IDKAMP         PIC X(7)    VALUE SPACE.                 
699399    10 WS-MSGI-RADNR             PIC 9(3) VALUE ZERO.                     
699799    10 FILLER                    PIC X(900)  VALUE SPACE.                 
699899                                                                          
700099     03  WS-TABELL-MAX           PIC 9(3)    VALUE 200.                   
710099     03  WS-TABELL.                                                       
710199      04 WS-TAB-POST    OCCURS 200.                                       
711099       05  WS-TAB-RAD.                                                    
720099        06  WS-IDKAMP-TAB        PIC X(7).                                
730099        06  WS-IDARTNR-TAB       PIC S9(9).                               
740099        06  WS-KVREPANT-TAB      PIC S9(3)V9(2).                          
760099        06  WS-KVKAMP-TOTAL-TAB  PIC S9(7).                               
770099        06  WS-KVKAMP-LAUNCH-TAB PIC S9(7).                               
780099        06  WS-KVKAMP-FIRST-TAB  PIC S9(7).                               
790099        06  WS-RERESPRT-TAB      PIC S9(1)V9(2).                          
800099       05  WS-TAB-SORT.                                                   
800199         06 WS-IDARTNR-TAB-SORT  PIC S9(9).                               
800599                                                                          
800699     03 WS-IDARTNR               PIC X(9)    VALUE SPACE.                 
800799     03 WS-IDARTNR-NUM           PIC 9(9)    VALUE ZERO.                  
800899     03 WS-AAVVD-NUM             PIC 9(5)    VALUE ZERO.                  
800999     03 WS-IDKAMP                PIC X(7)    VALUE SPACE.                 
801099     03 WS-IDKAMP-GRP            PIC S9(7)    VALUE ZERO.                 
801199     03 WS-IDKAMP-GRP-MAX        PIC S9(7)    VALUE ZERO COMP-3.          
801399     03 WS-TEMFSFEL              PIC X(40)   VALUE SPACE.                 
802099     03 WS-VISA-UNIK             PIC X(01)   VALUE SPACE.                 
803099     03 WS-VISA-DEF-ERS          PIC X(01)   VALUE SPACE.                 
804099     03 WS-IDKAMP-SPARA          PIC X(7)    VALUE SPACE.                 
805099     03 WS-KVKAMP-CARS-SPARA     PIC 9(7)    VALUE ZERO.                  
805199     03 WS-KVKAMP-TOTAL          PIC S9(7).                               
805299     03 WS-KVKAMP-LAUNCH         PIC S9(7).                               
805399     03 WS-KVKAMP-FIRST          PIC S9(7).                               
805499     03 WS-TOTAL                 PIC 9(7)    VALUE ZERO.                  
805599     03 WS-RERESPRT-SPARA        PIC S9(1)V9(2)                           
805699                                             VALUE ZERO.                  
805799     03 WS-KVREPANT-RED          PIC Z(2)9.9(2).                          
805899     03 WS-KVREPANT              PIC S9(3)V9(2)                           
805999                                             COMP-3 VALUE ZERO.           
806099     03 WS-RERESPRT              PIC S9(1)V9(2)                           
806199                                             COMP-3 VALUE ZERO.           
806299     03 WS-RERESPRT-NUM          PIC 9(3)    VALUE ZERO.                  
806399     03 WS-RERESPRT-RED          PIC ZZ9.                                 
806499     03 FILLER                   PIC X(16)   VALUE                        
806599                                             'WS-SQLCODE-SPARA'.          
806699     03 WS-SQLCODE-SPARA         PIC 9(3)    VALUE ZERO.                  
806799     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
807099     03 FILLER                   PIC X(16)   VALUE                        
807199                                             'WS-DB2-SEKTION'.            
808099     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
819099                                                                          
820099 77  UPD-RAD-SW                  PIC X       VALUE 'N'.                   
830099     88  UPD-RAD-JA                          VALUE 'J'.                   
840099     88  UPD-RAD-NEJ                         VALUE 'N'.                   
850099                                                                          
860099 77  REG-NY-RAD-SW               PIC X       VALUE 'N'.                   
870099     88  REG-NY-RAD-JA                       VALUE 'J'.                   
880099     88  REG-NY-RAD-NEJ                      VALUE 'N'.                   
940099                                                                          
980099     EJECT                                                                
990000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000000 01  GENERELLA-SUBPROGRAM.                                                
010000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
020000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
030000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
040099     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
050002     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
060099     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
070099     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
071099     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
080000     EJECT                                                                
130099*    --- PARAMETRAR TILL ABEND                                            
140099                                                                          
150099 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
160099 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
170099     SKIP2                                                                
180000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
190000*01 -COPY WMEDAREA                                                        
200099     EJECT                                                                
210099*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
220099*01  -COPY WDATAREA                                                       
230070     EJECT                                                                
240099*    --- PARAMETRAR TILL SUBPROGRAM WDAGKONV                              
250099*01  -COPY WDAGAREA                                                       
260099     EJECT                                                                
270099*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
280099*01  -COPY WMSGINIT                                                       
290099     EJECT                                                                
430199 01  MESSAGE-CODES.                                                       
430299     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
430399     03  CONFLICT                PIC X(3)    VALUE '002'.                 
430499     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
430599     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
430699     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
430799     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
430899     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
430999     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
431099     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
431199     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
431299     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
431399     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
431499     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
431599     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
431699     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
431799     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
431899 01  FELTEXTER.                                                           
434099     03  MED-1                  PIC X(40)                                 
436099         VALUE 'REMOVE NOTE ON 2313             '.                        
440099     EJECT                                                                
450000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
460000*                                                                         
470000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
480000     SKIP3                                                                
490099*01  MID -COPY W2I31601                                                   
500000     EJECT                                                                
510000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
520000     SKIP3                                                                
530000*01  -COPY WMSGAREA                                                       
540000     EJECT                                                                
550000     03  MOD REDEFINES MSG-AREA.                                          
560099*      05  -COPY W2O31601                                                 
570000     EJECT                                                                
580000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
590000     SKIP3                                                                
600000*01  -COPY WMFSAREA                                                       
610000     EJECT                                                                
620000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
630000*                                                                         
640000     EJECT                                                                
650000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
660000     SKIP3                                                                
670000 01  NYCKLAR-TILL-DLI.                                                    
680099     03  W-IDARTNR-X.                                                     
690099         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
700001                                                                          
740002     03  W-IDSKYLT-X.                                                     
750002         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
760099                                                                          
770099     03  W-KDSEGKEY-X.                                                    
780099         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
870099                                                                          
870199 01  FILLER                      PIC X(16)   VALUE 'NYCKLAR DB2'.         
870299     SKIP3                                                                
871099 01  NYCKLAR-TILL-DB2.                                                    
872099     03  W-IDKAMP                PIC X(7)    VALUE SPACE.                 
872299                                                                          
873099     03  W-IDKAMP-GRP            PIC S9(7)    VALUE ZERO COMP-3.          
874099                                                                          
875099     03  W-KVKAMP-LAUNCH         PIC S9(7)    VALUE ZERO COMP-3.          
875199                                                                          
875299     03  W-KVKAMP-FIRST          PIC S9(7)    VALUE ZERO COMP-3.          
875399                                                                          
875499     03  W-KVKAMP-TOTAL          PIC S9(7)    VALUE ZERO COMP-3.          
876099                                                                          
879199                                                                          
880000     SKIP2                                                                
881099*                                                                         
882099*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
883099*                                                                         
884099 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
884199 01  FILLER                  PIC X(16) VALUE 'TP1GRP-AREA'.               
885099*01  -COPY TP1GRP   -PRE TP1GRP-                                          
888099     EJECT                                                                
888199 01  FILLER                  PIC X(16) VALUE 'TP1KAMP-AREA'.              
888299*01  -COPY TP1KAMP  -PRE TP1KAMP-                                         
888399     EJECT                                                                
888499 01  FILLER                  PIC X(16) VALUE 'TP1ARTG-AREA'.              
888599*01  -COPY TP1ARTG  -PRE TP1ARTG-                                         
888699     EJECT                                                                
888799 01  FILLER                  PIC X(16) VALUE 'TP1ARTK-AREA'.              
888899*01  -COPY TP1ARTK  -PRE TP1ARTK-                                         
888999     EJECT                                                                
889199       EXEC SQL INCLUDE TP1GRP END-EXEC.                                  
889299                                                                          
889399       EXEC SQL INCLUDE TP1KAMP END-EXEC.                                 
889499                                                                          
889599       EXEC SQL INCLUDE TP1ARTG END-EXEC.                                 
889699                                                                          
889799       EXEC SQL INCLUDE TP1ARTK END-EXEC.                                 
889899                                                                          
889999 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
890099       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
890199     SKIP3                                                                
890399*                        **** STATUS-KOD FRÅN DB2                         
890499     EJECT                                                                
890799                                                                          
890899 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
890999 01  DB2-WS.                                                              
891099     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
891199         88  CURSOR-OK                       VALUE 000.                   
891299         88  LINES-FOUND                     VALUE 000.                   
891399         88  LINES-MISSING                   VALUE 100.                   
891499         88  LINES-IS-NULL                   VALUE 305.                   
891599         88  RESOURCE-WRONG                  VALUE 904.                   
891699     03  GODK-SQLCODESKODER.                                              
891799         05  GODK-SQLCODE OCCURS 5                                        
891899             INDEXED BY SQLCODE-IX PIC 9(3).                              
891999     EJECT                                                                
894000*    --- STATUS-KOD FRÅN IMS                                              
900000 01  STATUS-WS                   PIC XX.                                  
910000     88  SEGMENT-FINNS                       VALUE '  '.                  
920000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
930000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
940000     SKIP2                                                                
950000 01  GODK-STATUSKODER.                                                    
960000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
970000     SKIP3                                                                
980000 01  SSA1                        PIC X(64).                               
990000 01  SSA2                        PIC X(64).                               
000000     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
020000*01  -COPY W0003                                                          
250099     EJECT                                                                
670099                                                                          
680002 LINKAGE SECTION.                                                         
690000                                                                          
700000*01  -COPY W0009   -PRE MSG-                                              
710000     EJECT                                                                
720099*01  -COPY W0008  -PRE  USEA-                                             
730000     05  FILLER                  PIC X.                                   
950099     EJECT                                                                
960099 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB.                              
000099     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB.                              
040000                                                                          
050000     PERFORM IMS-GET-MSG                                                  
060000     IF SEGMENT-FINNS                                                     
070000       PERFORM A-INIT                                                     
080000       PERFORM B-KOLLA-NYCKLAR                                            
090000       IF NYCKLAR-OK                                                      
100000         IF MFS-UPDATE                                                    
110000           PERFORM G-KOLLA-INPUT                                          
120000           IF INDATA-OK                                                   
130000             PERFORM H-UPPDATERA                                          
140000           END-IF                                                         
150000         ELSE                                                             
201099           IF MFS-FIRST                                                   
202099              PERFORM C-FOERSTA-SIDA                                      
203099           ELSE                                                           
204099              IF MFS-NEXT                                                 
205099                 PERFORM D-NAESTA-SIDA                                    
206099              ELSE                                                        
207099                 PERFORM E-SAMMA-SIDA                                     
208099              END-IF                                                      
209099           END-IF                                                         
210000         END-IF                                                           
220099         IF INDATA-OK                                                     
220199           IF   WS-IDKAMP NOT = SPACE                                     
220599             MOVE WS-IDKAMP  TO W-IDKAMP                                  
220699             PERFORM DB2-SELECT-TP1KAMP                                   
220799             IF SQLCODE = ZERO                                            
220899             AND TP1KAMP-IDKAMP-GRP > ZERO                                
221099               MOVE TP1KAMP-IDKAMP-GRP                                    
221199                             TO WS-IDKAMP-GRP                             
221299               MOVE WS-IDKAMP-GRP                                         
221399                             TO MOD-IDKAMP-GRP-UT                         
221499               INSPECT MOD-IDKAMP-GRP-UT                                  
221599                             REPLACING LEADING ZERO BY SPACE              
221699               MOVE SPACE    TO WS-IDKAMP                                 
221799             END-IF                                                       
221899           END-IF                                                         
221999                                                                          
222199           IF WS-IDKAMP NOT = SPACE                                       
222399             MOVE ZERO       TO WS-IDKAMP-GRP                             
222499             MOVE MFS-RENSA-FAELT                                         
222599                             TO MOD-IDKAMP-GRP-UT                         
222699*                                                                         
222799* ATT LÄSA MED IDKAMP SOM NYCKEL                                          
222899*                                                                         
222999             PERFORM I-LAES-VISA-INFO                                     
223099           ELSE                                                           
223199             MOVE MFS-RENSA-FAELT                                         
223299                             TO MOD-IDKAMP-UT                             
224099*                                                                         
230399* ATT LÄSA MED IDKAMP-GRP SOM NYCKEL                                      
230499*                                                                         
231099             PERFORM F-LAES-VISA-INFO                                     
232099           END-IF                                                         
240099         END-IF                                                           
241099                                                                          
241199         MOVE ALL '+'        TO MSGI-WMSGINIT                             
241299         MOVE '001'          TO MSGI-KDCALL                               
241399         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
241499         MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                         
241599         MOVE '2316'         TO MSGI-IDTRANS                              
241899         MOVE WS-IDKAMP      TO MSGI-IDKAMP                               
241999         MOVE WS-IDKAMP-GRP  TO MSGI-IDKAMP-GRP                           
242399         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
242499                                                                          
242599* ---    UPPDATERA MSGI-SPAR-AREA                                         
243099         MOVE '002'          TO MSGI-KDCALL                               
244099         MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                         
245099         MOVE MSG-SIGNON-USERID                                           
246099                             TO MSGI-IDUSER                               
247099         MOVE '2316'         TO MSGI-IDTRANS                              
248099         MOVE WS-MSGI-AREA-2316                                           
249099                             TO MSGI-SPAR-AREA                            
249199         MOVE WS-IDKAMP      TO MSGI-IDKAMP                               
249299         MOVE WS-IDKAMP-GRP  TO MSGI-IDKAMP-GRP                           
249399         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
249499                                                                          
250000       END-IF                                                             
261099       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O31601 + 4                      
270000       PERFORM IMS-INSERT-MSG                                             
280000     END-IF                                                               
290000                                                                          
300000     MOVE ZERO TO RETURN-CODE                                             
310000     GOBACK                                                               
320000     .                                                                    
330000     EJECT                                                                
340000 A-INIT SECTION.                                                          
350000                                                                          
360000     IF MSG-DUBBLA-TRANSKODER                                             
370099       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I31601                 
380000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
390000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
400000     ELSE                                                                 
410099       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I31601                   
420000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
430000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
440000     END-IF                                                               
450000                                                                          
460000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
470000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
480000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
490000                                                                          
500000     MOVE LOW-VALUE  TO MSG-AREA                                          
510099     MOVE 'W2O316N1' TO MFS-IDMOD                                         
520099     MOVE '2316'     TO MOD-IDTRANS                                       
530000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
540099                                                                          
590059     IF EGEN-MID OR HELP-MID                                              
600059       CONTINUE                                                           
610059     ELSE                                                                 
620000       MOVE SPACE TO MFS-KDTRTYP                                          
630000       MOVE '7'   TO MFS-IDPFK                                            
640000     END-IF                                                               
641099     MOVE 'GB '              TO MED-IDSKYLT                               
650099                                                                          
651099     MOVE FUNCTION  CURRENT-DATE(1:8)  TO DAGENS-DATUM                    
811099     MOVE SPACE              TO WS-TEMFSFEL                               
830000     .                                                                    
840000     EJECT                                                                
850099 B-KOLLA-NYCKLAR SECTION.                                                 
860000                                                                          
861099     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
862099                                WS-MSGI-SSA-KEY-NEXT                      
863099                                                                          
864099******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
865099******   I SLUTET AV PROGRAMMET                                           
866099                                                                          
870000     MOVE JA TO NYCKLAR-SW                                                
880072                                                                          
890099*      -- KONTROLL AV IDKAMP                                              
900072                                                                          
910099     MOVE MFS-RENSA-FAELT TO MOD-IDKAMP-IN                                
920099                             MOD-IDKAMP-GRP-IN                            
930099                                                                          
940099     MOVE ALL '+' TO MSGI-WMSGINIT                                        
950099     MOVE '001'             TO MSGI-KDCALL                                
960099     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
970099     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
980099     MOVE '2316'            TO MSGI-IDTRANS                               
990099     IF EGEN-MID OR 2317-MID                                              
000099       MOVE MID-IDKAMP-IN    TO MSGI-IDKAMP                               
010099       MOVE MID-IDKAMP-GRP-IN                                             
020099                             TO MSGI-IDKAMP-GRP                           
021099     ELSE                                                                 
023199       MOVE ALL '+'          TO MID-IDKAMP-IN                             
023299                                MID-IDKAMP-GRP-IN                         
030099     END-IF                                                               
040099     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041099                                                                          
042099     IF EGEN-MID OR 2317-MID                                              
043099     AND MSGI-SPAR-AREA(1:4) = '2316'                                     
044099       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-2316                         
045099     END-IF                                                               
050099                                                                          
110099     MOVE +2      TO SPRAK-IX                                             
120099     MOVE 'GB '   TO MED-IDSKYLT                                          
140099                                                                          
150099     MOVE MSGI-IDKAMP        TO WS-IDKAMP                                 
151099                                MOD-IDKAMP-UT                             
153099                                                                          
160099     MOVE MSGI-IDKAMP-GRP    TO WS-IDKAMP-GRP                             
172099     INSPECT WS-IDKAMP-GRP REPLACING ALL SPACE BY ZERO                    
172199     MOVE WS-IDKAMP-GRP      TO MOD-IDKAMP-GRP-UT                         
172299     INSPECT MOD-IDKAMP-GRP-UT REPLACING LEADING ZERO BY SPACE            
180099                                                                          
190099     IF MID-IDKAMP-IN = ALL '+'                                           
200099       CONTINUE                                                           
210099     ELSE                                                                 
220099       MOVE '7'              TO MFS-IDPFK                                 
230099       MOVE SPACE            TO MFS-KDTRTYP                               
240099     END-IF                                                               
250099                                                                          
260099     IF MID-IDKAMP-GRP-IN = ALL '+'                                       
270099       CONTINUE                                                           
280099     ELSE                                                                 
290099       MOVE '7'              TO MFS-IDPFK                                 
300099       MOVE SPACE            TO MFS-KDTRTYP                               
301099       MOVE SPACE            TO WS-IDKAMP                                 
310099     END-IF                                                               
320099*                                                                         
330099     IF WS-IDKAMP-GRP NUMERIC                                             
340099       CONTINUE                                                           
350099     ELSE                                                                 
360099       MOVE NEJ TO NYCKLAR-SW                                             
370099     END-IF                                                               
380099                                                                          
800099                                                                          
810000     IF NYCKLAR-FEL                                                       
820000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
830000       CALL WMEDKONV USING MED-WMEDAREA                                   
840000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
850000       PERFORM MFS-RENSA-FAELT-UT                                         
860000     END-IF                                                               
870000     .                                                                    
880000     EJECT                                                                
890058 C-FOERSTA-SIDA SECTION.                                                  
891099                                                                          
893099     MOVE INF-FIRST-PAGE     TO MED-IDMFSFEL                              
894099     CALL WMEDKONV USING MED-WMEDAREA                                     
895099     MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                              
896099                                                                          
897099*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
898099     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
899099                                WS-MSGI-SSA-KEY-NEXT                      
899199     PERFORM MFS-RENSA-FAELT-IN                                           
920058     .                                                                    
930058     EJECT                                                                
931099 D-NAESTA-SIDA SECTION.                                                   
932099                                                                          
933099     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
934099     .                                                                    
935099     EJECT                                                                
940000 E-SAMMA-SIDA SECTION.                                                    
950000                                                                          
961099     IF MID-INPUT = ALL '+'                                               
970000       PERFORM MFS-RENSA-FAELT-IN                                         
980000     ELSE                                                                 
990031       IF EGEN-MID OR HELP-MID                                            
000000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
010000         CALL WMEDKONV USING MED-WMEDAREA                                 
021099         MOVE MED-TEMFSINF                                                
022099                         TO MOD-TEMFSINF                                  
030022         PERFORM MFS-LAES-IN-IGEN                                         
040020                                                                          
050036         PERFORM EA-MID-INDATA-TILL-MOD                                   
060000       ELSE                                                               
070000         PERFORM MFS-RENSA-FAELT-IN                                       
080000       END-IF                                                             
090000     END-IF                                                               
100000     .                                                                    
110036 EA-MID-INDATA-TILL-MOD SECTION.                                          
120036* * * * * FÖR VARJE MID-FÄLT                                              
130036* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
140036* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
150036                                                                          
160099     MOVE 1                  TO RAD-IX                                    
170099     PERFORM UNTIL RAD-IX > RAD-MAX                                       
180099                                                                          
190099       IF MID-CMD (RAD-IX) = ALL '+'                                      
200099         MOVE MFS-RENSA-FAELT                                             
210099                             TO MOD-CMD (RAD-IX)                          
220099       ELSE                                                               
230099         MOVE MID-CMD (RAD-IX)                                            
240099                             TO MOD-CMD (RAD-IX)                          
250099       END-IF                                                             
260099                                                                          
500099                                                                          
510099       ADD 1                 TO RAD-IX                                    
520099     END-PERFORM                                                          
530099                                                                          
540099     IF MID-IDKAMP-NY = ALL '+'                                           
550099       MOVE MFS-RENSA-FAELT  TO MOD-IDKAMP-NY                             
560099     ELSE                                                                 
570099       MOVE MID-IDKAMP-NY    TO MOD-IDKAMP-NY                             
590099     END-IF                                                               
600099                                                                          
880036     .                                                                    
890036     EJECT                                                                
891099 F-LAES-VISA-INFO SECTION.                                                
892099                                                                          
893099     MOVE 1                  TO RAD-IX                                    
895099     IF  WS-MSGI-SSA-KEY-ENTER = SPACE                                    
896099     AND WS-MSGI-SSA-KEY-NEXT  = SPACE                                    
897099       MOVE WS-IDKAMP        TO W-IDKAMP                                  
899099     ELSE                                                                 
899199       IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                               
899399         MOVE WS-ENTER-IDKAMP                                             
899499                             TO W-IDKAMP                                  
899599       ELSE                                                               
899799         MOVE WS-NEXT-IDKAMP                                              
899899                             TO W-IDKAMP                                  
899999       END-IF                                                             
900099     END-IF                                                               
900199     MOVE WS-IDKAMP-GRP      TO  W-IDKAMP-GRP                             
902799                                                                          
902899     PERFORM DB2-DCL-OPN-CRS-TP1KAMP                                      
902999     IF SQLCODE > ZERO                                                    
903099       MOVE INF-URVAL-SAKNAS                                              
903199                             TO MED-IDMFSFEL                              
903299       CALL WMEDKONV USING MED-WMEDAREA                                   
903399       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
903499                                                                          
903599     ELSE                                                                 
903699                                                                          
903799       PERFORM DB2-FETCH-TP1KAMP                                          
903899       PERFORM UNTIL SQLCODE > ZERO                                       
903999       OR RAD-IX > RAD-MAX                                                
904099         MOVE TP1KAMP-IDKAMP                                              
904199                             TO MOD-IDKAMP (RAD-IX)                       
904299         MOVE TP1KAMP-KDKAMP                                              
904399                             TO MOD-KDKAMP (RAD-IX)                       
904499         IF RAD-IX = 1                                                    
904599           MOVE TP1KAMP-IDKAMP                                            
904699                             TO WS-ENTER-IDKAMP                           
904799         END-IF                                                           
904899                                                                          
904999         ADD 1               TO RAD-IX                                    
905099         PERFORM DB2-FETCH-TP1KAMP                                        
905199       END-PERFORM                                                        
905299                                                                          
905399       IF SQLCODE = ZERO                                                  
905499          MOVE TP1KAMP-IDKAMP                                             
905599                             TO WS-NEXT-IDKAMP                            
905699          MOVE INF-MORE-INFO-EXISTS                                       
905799                             TO MED-IDMFSFEL                              
905899          CALL WMEDKONV USING MED-WMEDAREA                                
905999          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
906099       ELSE                                                               
906199          MOVE SPACE         TO WS-MSGI-SSA-KEY-NEXT                      
906299       END-IF                                                             
906399                                                                          
906499       PERFORM DB2-CLOSE-TP1KAMP-CRS                                      
906599     END-IF                                                               
906799                                                                          
906899     PERFORM UNTIL RAD-IX > RAD-MAX                                       
906999       MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR           (RAD-IX)           
907099       MOVE MFS-RENSA-FAELT  TO MOD-CMD           (RAD-IX)                
907199                                MOD-IDKAMP        (RAD-IX)                
907299                                MOD-KDKAMP        (RAD-IX)                
907399                                                                          
907499       ADD 1                 TO RAD-IX                                    
907599     END-PERFORM                                                          
907699     .                                                                    
907799     EJECT                                                                
120099 G-KOLLA-INPUT SECTION.                                                   
130099     MOVE 'G-KOLLA-INPUT      ' TO WS-SECTION                             
160099                                                                          
170099     MOVE JA                 TO INDATA-SW                                 
180099     MOVE NEJ                TO UPD-RAD-SW                                
190099                                REG-NY-RAD-SW                             
200099                                                                          
210099     IF  MID-INPUT      = ALL '+'                                         
220099        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
230099        MOVE NEJ TO INDATA-SW                                             
240099     ELSE                                                                 
250099                                                                          
253099                                                                          
260099       MOVE +1               TO RAD-IX                                    
270099       PERFORM UNTIL RAD-IX > RAD-MAX                                     
280099                                                                          
290099         IF MID-CMD             (RAD-IX) NOT = ALL '+'                    
300099           MOVE JA           TO UPD-RAD-SW                                
310099                                                                          
340099           IF       (MID-CMD (RAD-IX) = 'B'                               
350099           OR        MID-CMD (RAD-IX) = 'D')                              
360099                                                                          
370099             MOVE JA         TO UPD-RAD-SW                                
380099             MOVE MFS-ALFA-FAELT-RAETT                                    
390099                             TO MOD-CMD-ATTR(RAD-IX)                      
400099           ELSE                                                           
410099               MOVE MFS-ALFA-FAELT-FEL                                    
420099                             TO MOD-CMD-ATTR (RAD-IX)                     
430099               MOVE ERR-CORR-HILITE-FLDS                                  
440099                             TO MED-IDMFSFEL                              
450099               MOVE NEJ      TO INDATA-SW                                 
460099           END-IF                                                         
991099         END-IF                                                           
000099                                                                          
010099         ADD 1 TO RAD-IX                                                  
020099       END-PERFORM                                                        
030099                                                                          
040199       IF MID-IDKAMP-NY NOT = ALL '+'                                     
041199         MOVE MID-IDKAMP-NY  TO W-IDKAMP                                  
042099         PERFORM DB2-SELECT-TP1KAMP                                       
044099         IF   SQLCODE = ZERO                                              
044199         AND  TP1KAMP-KDKAMP = 'W'                                        
045099         AND  TP1KAMP-IDKAMP-GRP = ZERO                                   
045199         AND  TP1KAMP-BEKAMNOT = SPACE                                    
050099           MOVE JA           TO REG-NY-RAD-SW                             
059299                                                                          
059399         ELSE                                                             
059499           IF  SQLCODE = ZERO                                             
059599           AND TP1KAMP-BEKAMNOT NOT = SPACE                               
059699             MOVE MFS-ALFA-FAELT-FEL                                      
059799                             TO MOD-IDKAMP-NY-ATTR                        
059899             MOVE MED-1      TO WS-TEMFSFEL                               
059999             MOVE NEJ TO INDATA-SW                                        
060099           ELSE                                                           
071099             MOVE MFS-ALFA-FAELT-FEL                                      
072099                             TO MOD-IDKAMP-NY-ATTR                        
073099             MOVE ERR-CORR-HILITE-FLDS                                    
073199                             TO MED-IDMFSFEL                              
074099             MOVE NEJ TO INDATA-SW                                        
080099           END-IF                                                         
080199         END-IF                                                           
081099       ELSE                                                               
082099         IF UPD-RAD-NEJ                                                   
083099            MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                     
084099            MOVE NEJ         TO INDATA-SW                                 
085099         END-IF                                                           
090099       END-IF                                                             
100099     END-IF                                                               
110099                                                                          
111099     IF INDATA-FEL                                                        
112099        IF WS-TEMFSFEL = SPACE                                            
113099          CALL WMEDKONV USING MED-WMEDAREA                                
114099          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
115099        ELSE                                                              
116099          MOVE WS-TEMFSFEL  TO MOD-TEMFSFEL                               
117099        END-IF                                                            
118099        PERFORM MFS-ROER-EJ-FAELT-UT                                      
119099        PERFORM MFS-ROER-EJ-FAELT-IN                                      
170099     ELSE                                                                 
180099                                                                          
190099         IF UPD-RAD-JA                                                    
200099         AND REG-NY-RAD-JA                                                
210099*****                                                                     
220099*****     INTE MÖJLIGT ATT UPPDATERA BÅDE ENSKILD RAD                     
230099*****     OCH REGISTRERA NY ARTIKEL TILL EN KAMPANJ                       
240099*****                                                                     
250099             MOVE NEJ     TO INDATA-SW                                    
260099             MOVE CONFLICT TO MED-IDMFSFEL                                
270099             CALL WMEDKONV USING MED-WMEDAREA                             
280099             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
290099             PERFORM MFS-ROER-EJ-FAELT-IN                                 
300099             PERFORM MFS-ROER-EJ-FAELT-UT                                 
310099         END-IF                                                           
320099     END-IF                                                               
330099     .                                                                    
340099     EJECT                                                                
350099                                                                          
360099 H-UPPDATERA SECTION.                                                     
370099     MOVE 'H-UPPDATERA'      TO WS-SECTION                                
380099                                                                          
390099     IF UPD-RAD-JA                                                        
400099                                                                          
410099       MOVE +1 TO IX                                                      
420099       PERFORM UNTIL IX > RAD-MAX                                         
431099          IF MID-CMD            (IX) NOT = ALL '+'                        
440099             PERFORM HA-UPD-RAD                                           
450099          END-IF                                                          
460099          ADD 1 TO IX                                                     
470099       END-PERFORM                                                        
480099                                                                          
550099     ELSE                                                                 
560099                                                                          
570099       PERFORM HB-NY-RAD                                                  
580099     END-IF                                                               
590099                                                                          
600099     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
610099     CALL WMEDKONV USING MED-WMEDAREA                                     
620099     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
630099     PERFORM MFS-RENSA-FAELT-IN                                           
640099     .                                                                    
650099     EJECT                                                                
660099 HA-UPD-RAD SECTION.                                                      
670099     MOVE 'HA-UPD-RAD'       TO WS-SECTION                                
680099                                                                          
731099                                                                          
731399     MOVE MID-IDKAMP (IX)    TO W-IDKAMP                                  
732099     PERFORM DB2-SELECT-TP1KAMP                                           
733099     MOVE ZERO               TO TP1KAMP-IDKAMP-GRP                        
734099     PERFORM DB2-UPDATE-TP1KAMP                                           
470099     .                                                                    
480099     EJECT                                                                
490099 HB-NY-RAD SECTION.                                                       
500099     MOVE 'HB-NY-RAD'        TO WS-SECTION                                
510099                                                                          
511899*    MOVE MID-IDKAMP-NY      TO W-IDKAMP                                  
511999*    PERFORM DB2-SELECT-TP1KAMP                                           
512099                                                                          
512199     MOVE WS-IDKAMP-GRP      TO W-IDKAMP-GRP                              
512299     PERFORM DB2-SELECT-TP1GRP                                            
512399     MOVE SQLCODE            TO WS-SQLCODE-SPARA                          
512499     IF SQLCODE > ZERO                                                    
512699******  HÄMTA NYTT IDKAMP-GRP MHA HÖGSTA IDKAMP-GRP                       
512799                                                                          
512899       PERFORM DB2-SELECT-TP1GRP-MAX                                      
512999       IF LINES-IS-NULL                                                   
513099         MOVE ZERO TO WS-IDKAMP-GRP-MAX                                   
513199       END-IF                                                             
513299       COMPUTE TP1GRP-IDKAMP-GRP = WS-IDKAMP-GRP-MAX + 1                  
513399*      MOVE 113              TO TP1GRP-IDKAMP-GRP                         
513499       MOVE SPACE            TO TP1GRP-BEKAMNOT                           
513599       MOVE ZERO             TO TP1GRP-RERESPRT                           
513699       PERFORM DB2-INSERT-TP1GRP                                          
513799     END-IF                                                               
513899     MOVE TP1GRP-IDKAMP-GRP  TO TP1KAMP-IDKAMP-GRP                        
513999     PERFORM DB2-UPDATE-TP1KAMP                                           
514099     .                                                                    
514199     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
515099                                WS-MSGI-SSA-KEY-NEXT                      
840099     .                                                                    
860099     EJECT                                                                
871099 I-LAES-VISA-INFO SECTION.                                                
880099                                                                          
881099     MOVE 1                  TO RAD-IX                                    
900399     IF  WS-MSGI-SSA-KEY-ENTER = SPACE                                    
900499     AND WS-MSGI-SSA-KEY-NEXT  = SPACE                                    
900599       MOVE WS-IDKAMP        TO W-IDKAMP                                  
920099     ELSE                                                                 
921099       IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                               
940099         MOVE WS-ENTER-IDKAMP                                             
950099                             TO W-IDKAMP                                  
960099       ELSE                                                               
970099         MOVE WS-NEXT-IDKAMP                                              
980099                             TO W-IDKAMP                                  
990099       END-IF                                                             
000099     END-IF                                                               
020099     PERFORM DB2-SELECT-TP1KAMP                                           
131099     IF SQLCODE > ZERO                                                    
132099       MOVE INF-URVAL-SAKNAS                                              
133099                             TO MED-IDMFSFEL                              
134099       CALL WMEDKONV USING MED-WMEDAREA                                   
135099       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
136099                                                                          
137099     ELSE                                                                 
150099                                                                          
150199       IF TP1KAMP-KDKAMP = 'Q'                                            
150299       OR TP1KAMP-KDKAMP = 'S'                                            
150499         MOVE 'Q/S-KAMPANJ '                                              
150799                           TO MOD-TEMFSFEL                                
150899                                                                          
150999       ELSE                                                               
151099                                                                          
151199         IF TP1KAMP-IDKAMP-GRP = ZERO                                     
151399           MOVE WS-IDKAMP    TO MOD-IDKAMP-NY                             
151499           MOVE MFS-ADD-LAES-IN-FAELT                                     
151599                             TO MOD-IDKAMP-NY-ATTR                        
151699           MOVE 'GROUP MISSING'                                           
151799                             TO MOD-TEMFSFEL                              
151899                                                                          
151999         ELSE                                                             
152199                                                                          
152299           MOVE TP1KAMP-IDKAMP-GRP                                        
152399                             TO W-IDKAMP-GRP                              
153099           PERFORM DB2-DCL-OPN-CRS-TP1KAMP                                
160099           PERFORM DB2-FETCH-TP1KAMP                                      
170099           PERFORM UNTIL SQLCODE > ZERO                                   
183099           OR RAD-IX > RAD-MAX                                            
185299             MOVE TP1KAMP-IDKAMP                                          
185399                             TO MOD-IDKAMP (RAD-IX)                       
185499             MOVE TP1KAMP-KDKAMP                                          
185599                             TO MOD-KDKAMP (RAD-IX)                       
188099             IF RAD-IX = 1                                                
188199               MOVE TP1KAMP-IDKAMP                                        
188299                             TO WS-ENTER-IDKAMP                           
188399             END-IF                                                       
195999                                                                          
196099             ADD 1           TO RAD-IX                                    
196199             PERFORM DB2-FETCH-TP1KAMP                                    
197099           END-PERFORM                                                    
610099                                                                          
611099           IF SQLCODE = ZERO                                              
612099              MOVE TP1KAMP-IDKAMP                                         
630099                                 TO WS-NEXT-IDKAMP                        
641099              MOVE INF-MORE-INFO-EXISTS                                   
642099                                 TO MED-IDMFSFEL                          
643099              CALL WMEDKONV USING MED-WMEDAREA                            
644099              MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                           
645099           ELSE                                                           
646099              MOVE SPACE     TO WS-MSGI-SSA-KEY-NEXT                      
647099           END-IF                                                         
648099                                                                          
650099           PERFORM DB2-CLOSE-TP1KAMP-CRS                                  
651099         END-IF                                                           
652099       END-IF                                                             
660099                                                                          
670099     END-IF                                                               
680099                                                                          
690099     PERFORM UNTIL RAD-IX > RAD-MAX                                       
730099       MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR           (RAD-IX)           
770099       MOVE MFS-RENSA-FAELT  TO MOD-CMD           (RAD-IX)                
771099                                MOD-IDKAMP        (RAD-IX)                
772099                                MOD-KDKAMP        (RAD-IX)                
810099                                                                          
820099       ADD 1                 TO RAD-IX                                    
840099     END-PERFORM                                                          
032099     .                                                                    
033099     EJECT                                                                
120000 MFS-RENSA-FAELT-UT SECTION.                                              
130000                                                                          
140000*    --- ALLA UTDATA-FÄLT                                                 
471099     MOVE +1 TO IX                                                        
472099     PERFORM UNTIL IX > RAD-MAX                                           
473099       MOVE MFS-RENSA-FAELT  TO MOD-IDKAMP  (IX)                          
474099                                MOD-KDKAMP  (IX)                          
478099       ADD +1 TO IX                                                       
479099     END-PERFORM                                                          
479199     MOVE MFS-RENSA-FAELT    TO MOD-TEMFSINF                              
480000     .                                                                    
490000     SKIP2                                                                
500000 MFS-RENSA-FAELT-IN SECTION.                                              
510000                                                                          
511099*    --- ALLA INDATA-FÄLT                                                 
512099     MOVE +1 TO IX                                                        
513099     PERFORM UNTIL IX > RAD-MAX                                           
514099       MOVE MFS-RENSA-FAELT  TO MOD-CMD (IX)                              
518099       ADD +1 TO IX                                                       
519099     END-PERFORM                                                          
519199     MOVE MFS-RENSA-FAELT    TO MOD-IDKAMP-NY                             
640000     .                                                                    
650000     EJECT                                                                
660000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
670000                                                                          
670199*    --- ALLA UTDATA-FÄLT                                                 
670299     MOVE +1 TO IX                                                        
670399     PERFORM UNTIL IX > RAD-MAX                                           
670499       MOVE MFS-ROER-EJ-FAELT                                             
670599                             TO MOD-IDKAMP (IX)                           
671099       ADD +1 TO IX                                                       
671199     END-PERFORM                                                          
671299     MOVE MFS-ROER-EJ-FAELT  TO MOD-TEMFSINF                              
030000     .                                                                    
040000     SKIP2                                                                
050000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
060000                                                                          
061099*    --- ALLA INDATA-FÄLT                                                 
062099     MOVE +1 TO IX                                                        
063099     PERFORM UNTIL IX > RAD-MAX                                           
063199       MOVE MFS-ROER-EJ-FAELT                                             
064099                             TO MOD-CMD (IX)                              
069099       ADD +1 TO IX                                                       
069199     END-PERFORM                                                          
069299     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKAMP-NY                             
190000     .                                                                    
200000     EJECT                                                                
370000 MFS-LAES-IN-IGEN SECTION.                                                
380000                                                                          
381099*    --- ALLA INDATA-FÄLT                                                 
382099     MOVE +1 TO IX                                                        
383099     PERFORM UNTIL IX > RAD-MAX                                           
383199       MOVE MFS-ADD-LAES-IN-FAELT                                         
385099                             TO MOD-CMD-ATTR (IX)                         
389199       ADD +1 TO IX                                                       
389299     END-PERFORM                                                          
389499     MOVE MFS-ADD-LAES-IN-FAELT                                           
389599                             TO MOD-IDKAMP-NY-ATTR                        
510000     .                                                                    
520000     EJECT                                                                
530000* --- IMS SEKTIONER ---                                                   
540000     SKIP3                                                                
550000 IMS-GET-MSG SECTION.                                                     
560000                                                                          
570000     MOVE '  QC' TO GODK-STATUSKODER                                      
580000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
590000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
600000     PERFORM IMS-STATUSKONTROLL                                           
610000     .                                                                    
620000     SKIP3                                                                
630000 IMS-INSERT-MSG SECTION.                                                  
640000                                                                          
680000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
690000     MOVE SPACE TO GODK-STATUSKODER                                       
700000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
710000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
720000     PERFORM IMS-STATUSKONTROLL                                           
980099     .                                                                    
990099     EJECT                                                                
750000 IMS-STATUSKONTROLL SECTION.                                              
760000                                                                          
770000     SET STATUS-IX TO 1                                                   
780000     SEARCH GODK-STATUS                                                   
790000       AT END                                                             
800001         STRING 'OTILLÅTEN STATUSKOD FRÅN IMS: ' STATUS-WS                
810000         DELIMITED BY SIZE INTO FELTEXT                                   
820000         CALL FELLOG                                                      
830000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
840000         CONTINUE                                                         
850000     END-SEARCH                                                           
860000     .                                                                    
580099     EJECT                                                                
610099 DB2-DCL-OPN-CRS-TP1KAMP SECTION.                                         
620099     MOVE 'DB2-DCL-OPN-CRS-TP1KAMP' TO  WS-DB2-SEKTION                    
630099*    DISPLAY WS-DB2-SEKTION                                               
640099* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
650099     EXEC SQL DECLARE TP1KAMP-CRS CURSOR FOR                              
660099              SELECT IDKAMP,                                              
670099                     KVKAMP_CARS,                                         
680099                     RERESPRT,                                            
681099                     KDKAMP                                               
690099              FROM TP1KAMP                                                
700099              WHERE IDKAMP_GRP = :W-IDKAMP-GRP                            
701099              AND   IDKAMP    >= :W-IDKAMP                                
710099     END-EXEC                                                             
720099     MOVE 000               TO GODK-SQLCODESKODER                         
730099     EXEC SQL OPEN TP1KAMP-CRS END-EXEC                                   
740099     MOVE SQLCODE           TO SQLCODE-WS                                 
750099     PERFORM DB2-STATUSKONTROLL                                           
760099     .                                                                    
770099     EJECT                                                                
780099 DB2-FETCH-TP1KAMP SECTION.                                               
790099     MOVE 'DB2-FETCH-TP1KAMP' TO  WS-DB2-SEKTION                          
800099*    DISPLAY WS-DB2-SEKTION                                               
810099     MOVE 000100            TO GODK-SQLCODESKODER                         
820099     EXEC SQL FETCH TP1KAMP-CRS INTO                                      
830099            :TP1KAMP-IDKAMP                                               
840099           ,:TP1KAMP-KVKAMP-CARS                                          
850099           ,:TP1KAMP-RERESPRT                                             
851099           ,:TP1KAMP-KDKAMP                                               
860099     END-EXEC                                                             
870099     MOVE SQLCODE           TO SQLCODE-WS                                 
880099     PERFORM DB2-STATUSKONTROLL                                           
890099     .                                                                    
900099     EJECT                                                                
910099 DB2-CLOSE-TP1KAMP-CRS SECTION.                                           
920099     MOVE 'DB2-CLOSE-TP1KAMP-CRS' TO  WS-DB2-SEKTION                      
930099*    DISPLAY WS-DB2-SEKTION                                               
940099     SKIP2                                                                
950099     EXEC SQL CLOSE TP1KAMP-CRS END-EXEC                                  
960099     .                                                                    
970099     EJECT                                                                
430099 DB2-SELECT-TP1KAMP     SECTION.                                          
431099     MOVE 'DB2-SELECT-TP1KAMP   ' TO  WS-DB2-SEKTION                      
440099                                                                          
450099     MOVE 000100 TO GODK-SQLCODESKODER                                    
460099                                                                          
470099     EXEC SQL                                                             
480099           SELECT  IDKAMP                                                 
490099                  ,KVKAMP_CARS                                            
490199                  ,RERESPRT                                               
490299                  ,KDKAMP                                                 
491099                  ,BEKAMNOT                                               
492099                  ,IDKAMP_GRP                                             
500099                                                                          
510099           INTO   :TP1KAMP-IDKAMP                                         
520099                 ,:TP1KAMP-KVKAMP-CARS                                    
520199                 ,:TP1KAMP-RERESPRT                                       
520299                 ,:TP1KAMP-KDKAMP                                         
520399                 ,:TP1KAMP-BEKAMNOT                                       
520499                 ,:TP1KAMP-IDKAMP-GRP                                     
530099                                                                          
540099           FROM    TP1KAMP                                                
550099                                                                          
560099           WHERE   IDKAMP    = :W-IDKAMP                                  
570099     END-EXEC                                                             
580099                                                                          
590099     MOVE SQLCODE TO SQLCODE-WS                                           
600099     PERFORM DB2-STATUSKONTROLL                                           
610099     .                                                                    
620099     EJECT                                                                
930099 DB2-UPDATE-TP1KAMP  SECTION.                                             
930199     MOVE 'DB2-UPDATE-TP1KAMP   ' TO  WS-DB2-SEKTION                      
930299                                                                          
930399     MOVE 000     TO GODK-SQLCODESKODER                                   
930499     EXEC SQL                                                             
930599         UPDATE TP1KAMP                                                   
930699             SET IDKAMP_GRP    = :TP1KAMP-IDKAMP-GRP                      
933399                                                                          
933499         WHERE   IDKAMP    = :W-IDKAMP                                    
934099     END-EXEC                                                             
934199                                                                          
934299     MOVE SQLCODE TO SQLCODE-WS                                           
934399     PERFORM DB2-STATUSKONTROLL                                           
934499     .                                                                    
934599     EJECT                                                                
934799 DB2-SELECT-TP1GRP      SECTION.                                          
934899     MOVE 'DB2-SELECT-TP1GRP   ' TO   WS-DB2-SEKTION                      
934999                                                                          
935099     MOVE 000100 TO GODK-SQLCODESKODER                                    
935199                                                                          
935299     EXEC SQL                                                             
935399           SELECT  IDKAMP_GRP                                             
935499                  ,BEKAMNOT                                               
935599                  ,RERESPRT                                               
935899                                                                          
935999           INTO   :TP1GRP-IDKAMP-GRP                                      
936099                 ,:TP1GRP-BEKAMNOT                                        
936199                 ,:TP1GRP-RERESPRT                                        
936499                                                                          
936599           FROM    TP1GRP                                                 
936699                                                                          
936799           WHERE   IDKAMP_GRP = :W-IDKAMP-GRP                             
936899     END-EXEC                                                             
936999                                                                          
937099     MOVE SQLCODE TO SQLCODE-WS                                           
937199     PERFORM DB2-STATUSKONTROLL                                           
937299     .                                                                    
937399     EJECT                                                                
937499 DB2-SELECT-TP1GRP-MAX  SECTION.                                          
937599     MOVE 'DB2-SELECT-TP1GRP-MAX' TO   WS-DB2-SEKTION                     
937699                                                                          
937799     MOVE 000100305 TO GODK-SQLCODESKODER                                 
937899                                                                          
937999     EXEC SQL                                                             
938099           SELECT  MAX(IDKAMP_GRP)                                        
938399                                                                          
938499           INTO   :WS-IDKAMP-GRP-MAX                                      
938799                                                                          
938899           FROM    TP1GRP                                                 
939199     END-EXEC                                                             
939299                                                                          
939399     MOVE SQLCODE TO SQLCODE-WS                                           
939499     PERFORM DB2-STATUSKONTROLL                                           
939599     .                                                                    
939699     EJECT                                                                
939799 DB2-INSERT-TP1GRP  SECTION.                                              
939899     MOVE 'DB2-INSERT-TP1GRP   ' TO  WS-DB2-SEKTION                       
939999     SKIP2                                                                
940099     MOVE 000   TO GODK-SQLCODESKODER                                     
940199     EXEC SQL                                                             
940299         INSERT INTO TP1GRP                                               
940399            (IDKAMP_GRP,BEKAMNOT,RERESPRT)                                
940499         VALUES                                                           
940599            (:TP1GRP-IDKAMP-GRP,:TP1GRP-BEKAMNOT                          
940699            ,:TP1GRP-RERESPRT)                                            
940799     END-EXEC                                                             
940899                                                                          
940999     MOVE SQLCODE TO SQLCODE-WS                                           
941099     PERFORM DB2-STATUSKONTROLL                                           
941199     .                                                                    
941299     EJECT                                                                
941399 DB2-STATUSKONTROLL  SECTION.                                             
941499                                                                          
941599     SET SQLCODE-IX TO 1                                                  
941699     SEARCH GODK-SQLCODE                                                  
941799       AT END CALL FELLOG                                                 
941899       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
941999     END-SEARCH                                                           
942099     .                                                                    
