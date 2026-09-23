000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W9801000.                                                 
000400 AUTHOR.        MARGARETA-GABRIELSSON.                                    
000500 DATE-WRITTEN.  OKTOBER 1984.                                             
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      PROGRAMMET SKAPAR EN KALENDERFIL TILL SOP MED UT-                  
001100*      GÅNGSPUNKT FRÅN ETT GIVET DATUMINTERVALL (FR O M -                 
001200*      T O M).                                                            
001300*                                                                         
001400*      KALENDERFILEN HAR DATUM AAMMDD I POS 1-6 FÖLJT AV                  
001500*      ETT FRITT ANTAL KATALOGORD.                                        
001600*      ETT DATUM KAN GE UPPHOV TILL EN POST MED ETT ELLER                 
001700*      FLERA KATALOGORD, BEROENDE PÅ VILKEN 'TYP AV DAG' DET              
001800*      ÄR.                                                                
001900*                                                                         
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 INPUT-OUTPUT SECTION.                                                    
002400*                                                                         
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*--- UTFIL:                                                               
002800     SELECT CALENDAR                     ASSIGN TO W98010D1.              
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  CALENDAR                                                             
003500     LABEL RECORD   STANDARD                                              
003600     RECORDING      F                                                     
003700     BLOCK CONTAINS 0.                                                    
003800     SKIP2                                                                
003900 01  CALENDAR-POST   PIC X(250).                                          
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200*    -COPY WY2000W1                                                       
004300     SKIP3                                                                
004400*                            GENERERAT PROGRAM-NAMN                       
004500 77   PROGRAM-NAMN           VALUE 'W9801000'                             
004600                                 PIC X(8).                                
004700                                                                          
004800*                            GENERELLA KONSTANTER                         
004900*                                                                         
005000 77  JA                          PIC X(1)    VALUE 'J'.                   
005100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005200     SKIP3                                                                
005300*                            INDEXAR                                      
005400*                                                                         
005500 01  TIX                         PIC 9(9)    COMP SYNC.                   
005600 01  TIX-MAX                     PIC 9(9)    COMP SYNC.                   
005700 01  IX                          PIC 9(9)    COMP SYNC.                   
005800 01  IX-MAX                      PIC 9(9)    COMP SYNC.                   
005900 01  AIX                         PIC 9(9)    COMP SYNC.                   
006000 01  BIX                         PIC 9(9)    COMP SYNC.                   
006100 01  TID                         PIC 9(9)    COMP SYNC.                   
006200                                                                          
006300     EJECT                                                                
006400*                            DIVERSE ARBETSFÄLT                           
006500                                                                          
006600 01  W-DATUM                PIC 9(6).                                     
006700 01  FILLER REDEFINES W-DATUM.                                            
006800     03  W-AAR              PIC 9(2).                                     
006900     03  W-MANAD            PIC 9(2).                                     
007000     03  W-DAG              PIC 9(2).                                     
007100                                                                          
007200                                                                          
007300 01  W-SISTA-I-MANAD        PIC 9(2)  COMP-3.                             
007400                                                                          
007500                                                                          
007600 01  FILLER.                                                              
007700     03  DAG-TABELL OCCURS 500.                                           
007800       05  TAB-ARB-RAD.                                                   
007900           07  TAB-TID            PIC 9.                                  
008000           07  TAB-TIVV           PIC 9(2).                               
008100           07  TAB-TIMM           PIC 9(2).                               
008200           07  TAB-TIP            PIC 9.                                  
008300           07  TAB-TIRP           PIC 9(2).                               
008400           07  TAB-TIPP           PIC 9(2).                               
008500           07  TAB-KVARBDAG       PIC 9(3).                               
008600       05  TAB-UT-RAD.                                                    
008700           07  TAB-TIDATUM        PIC 9(6).                               
008800           07  FILLER REDEFINES TAB-TIDATUM.                              
008900               09  TAB-TIAAR      PIC 9(2).                               
009000               09  TAB-TIMMDD.                                            
009100                 11  TAB-TIMANAD    PIC 9(2).                             
009200                 11  TAB-TIDAG      PIC 9(2).                             
009300           07  TAB-KAT-ORD        PIC X(300).                             
009400     EJECT                                                                
009500 01  ORD                    PIC X(10).                                    
009600 01  WORD-1                 PIC X(20).                                    
009700 01  WORD-2                 PIC X(20).                                    
009800 01  WORD-3                 PIC X(20).                                    
009900 01  WORD-4                 PIC X(20).                                    
010000 01  WORD-5                 PIC X(20).                                    
010100 01  WORD-6                 PIC X(20).                                    
010200 01  WORD-7                 PIC X(20).                                    
010300 01  WORD-8                 PIC X(20).                                    
010400 01  WORD-9                 PIC X(20).                                    
010500 01  WORD-10                PIC X(20).                                    
010600 01  WORD-11                PIC X(20).                                    
010700 01  WORD-12                PIC X(20).                                    
010800                                                                          
010900 01  KATALOG-ORD.                                                         
011000     03  DAG                PIC X(3)  VALUE 'DAY'.                        
011100     03  IMS                PIC X(3)  VALUE 'IMS'.                        
011200     03  VECKA              PIC X(4)  VALUE 'WEEK'.                       
011300     03  VECKODAG           PIC X(3)  VALUE SPACE.                        
011400     03  MANAD              PIC X(5)  VALUE 'MONTH'.                      
011500     03  MANADSNAMN         PIC X(3)  VALUE SPACE.                        
011600     03  PLAN-PERIOD        PIC X(7)  VALUE 'PROGRAM'.                    
011700     03  REDO-PERIOD        PIC X(7)  VALUE 'ACCOUNT'.                    
011800     03  LVPLAN-PERIOD      PIC X(9)  VALUE 'LVPROGRAM'.                  
011900     03  PVPLAN-PERIOD      PIC X(9)  VALUE 'PVPROGRAM'.                  
012000     03  PVREDO-PERIOD      PIC X(9)  VALUE 'PVACCOUNT'.                  
012100     03  KVARTAL            PIC X(7)  VALUE 'QUARTER'.                    
012200     03  AAR                PIC X(4)  VALUE 'YEAR'.                       
012300     03  PLU                PIC X     VALUE '+'.                          
012400     03  MIN                PIC X     VALUE '-'.                          
012500     03  UDDA-VECKA         PIC X(7)  VALUE 'ODDWEEK'.                    
012600     03  JAMN-VECKA         PIC X(8)  VALUE 'EVENWEEK'.                   
012700     03  EFTER-VECKOSLUT    PIC X(9)  VALUE 'AFTERWEEK'.                  
012800                                                                          
012900 01  VECKDAG-TAB  PIC X(21) VALUE 'MONTUEWEDTHUFRISATSUN'.                
013000 01  FILLER REDEFINES VECKDAG-TAB.                                        
013100     03  VECKDAG OCCURS 7  PIC X(3).                                      
013200                                                                          
013300 01  IX-ZONAT                       PIC 9(1).                             
013400 01  IX-ZONAT-X REDEFINES IX-ZONAT  PIC X(1).                             
013500                                                                          
013600 01  PTR                         PIC 9(9)    COMP SYNC.                   
013700 01  START-PTR                   PIC 9(9)    COMP SYNC.                   
013800     EJECT                                                                
013900 01  START-POINTRAR.                                                      
014000     03  DATUM-PTR          PIC S9(9) COMP  VALUE +2.                     
014100     03  MANADSNAMN-PTR     PIC S9(9) COMP  VALUE +7.                     
014200     03  VECKODAG-PTR       PIC S9(9) COMP  VALUE +11.                    
014300     03  MANVECKDAG-PTR     PIC S9(9) COMP  VALUE +15.                    
014400     03  UJ-VECKA-PTR       PIC S9(9) COMP  VALUE +26.                    
014500     03  DAG-PTR            PIC S9(9) COMP  VALUE +36.                    
014600     03  IMS-PTR            PIC S9(9) COMP  VALUE +40.                    
014700     03  VECKOSTART-PTR     PIC S9(9) COMP  VALUE +44.                    
014800     03  VECKOSLUT-PTR      PIC S9(9) COMP  VALUE +50.                    
014900     03  PLANPER-PTR        PIC S9(9) COMP  VALUE +57.                    
015000     03  PLANPERVECK-PTR    PIC S9(9) COMP  VALUE +67.                    
015100     03  REDPER-PTR         PIC S9(9) COMP  VALUE +81.                    
015200     03  REDPERVECK-PTR     PIC S9(9) COMP  VALUE +92.                    
015300     03  MAN-PTR            PIC S9(9) COMP  VALUE +106.                   
015400     03  MANVECK-PTR        PIC S9(9) COMP  VALUE +114.                   
015500     03  KVARTAL-PTR        PIC S9(9) COMP  VALUE +126.                   
015600     03  KVARTALVECK-PTR    PIC S9(9) COMP  VALUE +136.                   
015700     03  AAR-PTR            PIC S9(9) COMP  VALUE +150.                   
015800     03  AARVECK-PTR        PIC S9(9) COMP  VALUE +157.                   
015900     03  AARMAN-PTR         PIC S9(9) COMP  VALUE +168.                   
016000     03  AARPLAN-PTR        PIC S9(9) COMP  VALUE +182.                   
016100     03  AARREDO-PTR        PIC S9(9) COMP  VALUE +196.                   
016200     03  PVPLAN-PTR         PIC S9(9) COMP  VALUE +210.                   
016300     03  PVREDO-PTR         PIC S9(9) COMP  VALUE +222.                   
016400     EJECT                                                                
016500 01  SPAR-DATUM.                                                          
016600     03  SPAR-DATUM-FOM     PIC 9(6).                                     
016700     03  SPAR-DATUM-TOM     PIC 9(6).                                     
016800     EJECT                                                                
016900 01  DYNAMISKA-SUBPROGRAM.                                                
017000*                                                                         
017100   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
017200   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
017300   03  WDAGKONV                  PIC X(8)    VALUE 'WDAGKONV'.            
017400     SKIP2                                                                
017500*******                      PARAMETRAR TILL ABEND                        
017600*                                                                         
017700 01  RETURKODER.                                                          
017800   03  RKOD                      PIC S9(4)   VALUE ZERO COMP SYNC.        
017900   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
018000   03  RKOD-ABEND-MED-DUMP       PIC S9(4)   VALUE +1000                  
018100                                                        COMP SYNC.        
018200     EJECT                                                                
018300*******                      PARAMETRAR TILL WDAGKONV                     
018400*                                                                         
018500*01  -COPY WDAGAREA                                                       
018600     EJECT                                                                
018700*******                      PARAMETRAR TILL WDATKONV                     
018800*                                                                         
018900*01  -COPY WDATAREA                                                       
019000     EJECT                                                                
019100                                                                          
019200 LINKAGE SECTION.                                                         
019300     SKIP2                                                                
019400 01  EXEC-PARM.                                                           
019500     03  LAENGD               PIC S9(4)   COMP.                           
019600     03  STARTA               PIC X(6).                                   
019700     03  STOPPA               PIC X(6).                                   
019800     EJECT                                                                
019900 PROCEDURE DIVISION USING EXEC-PARM.                                      
020000     SKIP2                                                                
020100     PERFORM A-INIT                                                       
020200     PERFORM B-MANIPULERA-INTERVALL                                       
020300     PERFORM C-SKAPA-TABELL-INOM-INTERVALL                                
020400*                                                                         
020500     MOVE 1 TO TIX                                                        
020600     PERFORM UNTIL TAB-TIDATUM (TIX) = SPAR-DATUM-FOM                     
020700         ADD 1 TO TIX                                                     
020800     END-PERFORM                                                          
020900     MOVE TAB-TIDATUM (TIX)  TO TMP1-YYMMDD                               
021000     MOVE SPAR-DATUM-TOM     TO TMP2-YYMMDD                               
021100     PERFORM WY2000P1                                                     
021200     PERFORM UNTIL TIX > 500 OR TMP1-YYMMDD > TMP2-YYMMDD                 
021300         PERFORM DB-TESTA-VECKODAG                                        
021400         PERFORM DN-TESTA-MANADSNAMN                                      
021500         PERFORM DO-GENERERA-DATUMET                                      
021600                                                                          
021700         IF TAB-KVARBDAG (TIX) = 1                                        
021800             PERFORM DA-ARBDAG                                            
021900                                                                          
022000             PERFORM S20-NAESTA-ARBDAG                                    
022100             PERFORM DC-TESTA-VECKOSLUT                                   
022200             PERFORM DE-TESTA-PLAN-PERIODSLUT                             
022300             PERFORM DG-TESTA-REDO-PERIODSLUT                             
022400             PERFORM DS-TESTA-KVARTALSSLUT                                
022500             PERFORM DK-TESTA-ARSSLUT                                     
022600             PERFORM DM-TESTA-PV-PLAN-PERIODSLUT                          
022700                                                                          
022800             PERFORM S21-FOREG-ARBDAG                                     
022900             PERFORM DD-TESTA-VECKOSTART                                  
023000             PERFORM DF-TESTA-PLAN-PERIODSTART                            
023100             PERFORM DH-TESTA-REDO-PERIODSTART                            
023200             PERFORM DT-TESTA-KVARTALSSTART                               
023300             PERFORM DL-TESTA-ARSSTART                                    
023400         END-IF                                                           
023500                                                                          
023600         IF TAB-TID (TIX) <= 6                                            
023700           PERFORM S25-NAESTA-ICKE-SONDAG                                 
023800           PERFORM DI-TESTA-MANADSSLUT                                    
023900                                                                          
024000           PERFORM S26-FOREG-ICKE-SONDAG                                  
024100           PERFORM DJ-TESTA-MANADSSTART                                   
024200         END-IF                                                           
024300                                                                          
024400         PERFORM S22-NAESTA-DAG                                           
024500         PERFORM DP-TESTA-VECKODAG-MANADSSLUT                             
024600                                                                          
024700         PERFORM S23-FOREG-DAG                                            
024800         PERFORM DQ-TESTA-VECKODAG-MANADSSTART                            
024900                                                                          
025000         PERFORM DR-UDDA-JAMN-VECKA                                       
025100                                                                          
025200         ADD 1 TO TIX                                                     
025300         IF TIX <= TIX-MAX                                                
025400           MOVE TAB-TIDATUM (TIX)  TO TMP1-YYMMDD                         
025500           MOVE SPAR-DATUM-TOM     TO TMP2-YYMMDD                         
025600           PERFORM WY2000P1                                               
025700         END-IF                                                           
025800     END-PERFORM                                                          
025900                                                                          
026000     PERFORM S04-SKRIV-HELA-CALENDAR                                      
026100                                                                          
026200     CLOSE CALENDAR                                                       
026300     MOVE 0 TO RETURN-CODE                                                
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 A-INIT SECTION.                                                          
026800     SKIP2                                                                
026900     OPEN OUTPUT CALENDAR                                                 
027000     MOVE 1 TO TIX                                                        
027100     PERFORM UNTIL TIX > 500                                              
027200         MOVE SPACE TO TAB-KAT-ORD (TIX)                                  
027300         ADD 1 TO TIX                                                     
027400     END-PERFORM                                                          
027500     .                                                                    
027600     EJECT                                                                
027700 B-MANIPULERA-INTERVALL SECTION.                                          
027800     SKIP2                                                                
027900*   RÄKNA UT ETT NYTT T O M-DATUM 10 DAGAR SENARE ÄN URSPRUNGLIGT         
028000                                                                          
028100     MOVE STOPPA TO SPAR-DATUM-TOM                                        
028200                   DAG-TIAAMMDD-FOM                                       
028300     MOVE 002 TO DAG-KDCALL                                               
028400     MOVE 10  TO DAG-KVKALDAG                                             
028500     PERFORM S01-ANROPA-WDAGKONV                                          
028600     MOVE DAG-TIAAMMDD-TOM TO STOPPA                                      
028700     SKIP3                                                                
028800*   RÄKNA UT ETT NYTT F O M-DATUM 10 DAGAR TIDIGARE ÄN URSPRUNGL.         
028900                                                                          
029000     MOVE STARTA TO SPAR-DATUM-FOM                                        
029100                   DAG-TIAAMMDD-TOM                                       
029200     MOVE 003 TO DAG-KDCALL                                               
029300     MOVE 10  TO DAG-KVKALDAG                                             
029400     PERFORM S01-ANROPA-WDAGKONV                                          
029500     MOVE DAG-TIAAMMDD-FOM TO STARTA                                      
029600     .                                                                    
029700     EJECT                                                                
029800 C-SKAPA-TABELL-INOM-INTERVALL SECTION.                                   
029900     SKIP2                                                                
030000     MOVE 1 TO TIX                                                        
030100     MOVE STARTA TO W-DATUM                                               
030200     MOVE W-DATUM    TO TMP1-YYMMDD                                       
030300     MOVE STOPPA     TO TMP2-YYMMDD                                       
030400     PERFORM WY2000P1                                                     
030500     PERFORM UNTIL TMP1-YYMMDD > TMP2-YYMMDD OR  TIX > 500                
030600*                                                                         
030700       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
030800       MOVE W-DATUM TO DAT-I-TIDATUM                                      
030900       PERFORM S02-ANROPA-WDATKONV                                        
031000*                                                                         
031100       MOVE DAT-TIAAMMDD TO TAB-TIDATUM (TIX)                             
031200       MOVE DAT-TID      TO TAB-TID (TIX)                                 
031300       MOVE DAT-TIVV     TO TAB-TIVV (TIX)                                
031400       MOVE DAT-TIMM     TO TAB-TIMM (TIX)                                
031500       MOVE DAT-TIP      TO TAB-TIP (TIX)                                 
031600       MOVE DAT-TIRP     TO TAB-TIRP (TIX)                                
031700       MOVE DAT-TIPP     TO TAB-TIPP (TIX)                                
031800       IF DAT-TID < 6                                                     
031900       AND TAB-TIMMDD (TIX) NOT = 1225 AND 1231 AND 0101                  
032000         MOVE 1 TO TAB-KVARBDAG (TIX)                                     
032100       ELSE                                                               
032200         MOVE 0 TO TAB-KVARBDAG (TIX)                                     
032300       END-IF                                                             
032400                                                                          
032500       MOVE TIX TO TIX-MAX                                                
032600*                                                                         
032700       ADD 1 TO TIX                                                       
032800*                                                                         
032900       PERFORM S03-OEKA-KOLLA-DATUM                                       
033000       MOVE W-DATUM    TO TMP1-YYMMDD                                     
033100       MOVE STOPPA     TO TMP2-YYMMDD                                     
033200       PERFORM WY2000P1                                                   
033300     END-PERFORM                                                          
033400     .                                                                    
033500     EJECT                                                                
033600 DA-ARBDAG SECTION.                                                       
033700     SKIP2                                                                
033800     MOVE DAG-PTR TO PTR                                                  
033900     STRING DAG DELIMITED BY SIZE                                         
034000     INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                              
034100                                                                          
034200     MOVE IMS-PTR TO PTR                                                  
034300     STRING IMS DELIMITED BY SIZE                                         
034400     INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                              
034500                                                                          
034600     IF TAB-TIVV (TIX) NOT = DAT-TIVV                                     
034700       MOVE IMS TO ORD                                                    
034800       MOVE IMS-PTR TO START-PTR                                          
034900       PERFORM S07-LORDAGS-ORD                                            
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 DB-TESTA-VECKODAG SECTION.                                               
035400     SKIP2                                                                
035500     MOVE TAB-TID (TIX) TO TID                                            
035600     MOVE VECKODAG-PTR TO PTR                                             
035700     STRING VECKDAG (TID) DELIMITED BY SIZE                               
035800     INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                              
035900                                                                          
036000     IF VECKDAG (TID) = 'SUN'                                             
036100       MOVE VECKOSLUT-PTR TO PTR                                          
036200       STRING EFTER-VECKOSLUT DELIMITED BY SIZE                           
036300       INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                            
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 DC-TESTA-VECKOSLUT SECTION.                                              
036800     SKIP2                                                                
036900     IF TAB-TIVV (TIX) NOT = DAT-TIVV                                     
037000        MOVE VECKA TO ORD                                                 
037100        MOVE VECKOSLUT-PTR TO START-PTR                                   
037200        MOVE 3 TO IX-MAX                                                  
037300        PERFORM S10-ORD-MINUS-IX-DAGAR                                    
037400                                                                          
037500        PERFORM S07-LORDAGS-ORD                                           
037600                                                                          
037700     END-IF                                                               
037800     .                                                                    
037900     SKIP3                                                                
038000     SKIP3                                                                
038100 DD-TESTA-VECKOSTART SECTION.                                             
038200     SKIP2                                                                
038300     IF TAB-TIVV (TIX) NOT = DAT-TIVV                                     
038400        MOVE VECKA TO ORD                                                 
038500        MOVE VECKOSTART-PTR TO START-PTR                                  
038600        MOVE 3 TO IX-MAX                                                  
038700        PERFORM S11-ORD-PLUS-IX-DAGAR                                     
038800                                                                          
038900*       -- LÄGG IN "IMS1" I VECKOSLUTSKOLUMNEN PÅ "WEEK1"-RADER           
039000        MOVE IMS   TO ORD                                                 
039100        MOVE VECKOSLUT-PTR TO START-PTR                                   
039200        MOVE 1 TO IX-MAX                                                  
039300        PERFORM S11-ORD-PLUS-IX-DAGAR                                     
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 DE-TESTA-PLAN-PERIODSLUT SECTION.                                        
039800     SKIP2                                                                
039900     IF TAB-TIP (TIX) NOT = DAT-TIP                                       
040000        MOVE PLAN-PERIOD TO ORD                                           
040100                                                                          
040200        MOVE PLANPER-PTR TO START-PTR                                     
040300        MOVE 5 TO IX-MAX                                                  
040400        PERFORM S10-ORD-MINUS-IX-DAGAR                                    
040500                                                                          
040600        PERFORM S07-LORDAGS-ORD                                           
040700                                                                          
040800        MOVE PLANPERVECK-PTR TO START-PTR                                 
040900        MOVE 2 TO IX-MAX                                                  
041000        PERFORM S12-ORD-MINUS-IX-VECKOR                                   
041100     END-IF                                                               
041200     .                                                                    
041300     SKIP3                                                                
041400     SKIP3                                                                
041500 DF-TESTA-PLAN-PERIODSTART SECTION.                                       
041600     SKIP2                                                                
041700     IF TAB-TIP (TIX) NOT = DAT-TIP                                       
041800        MOVE PLAN-PERIOD  TO ORD                                          
041900                                                                          
042000        MOVE PLANPER-PTR TO START-PTR                                     
042100        MOVE 5 TO IX-MAX                                                  
042200        PERFORM S11-ORD-PLUS-IX-DAGAR                                     
042300                                                                          
042400        MOVE PLANPERVECK-PTR TO START-PTR                                 
042500        MOVE 3 TO IX-MAX                                                  
042600        PERFORM S13-ORD-PLUS-IX-VECKOR                                    
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000 DG-TESTA-REDO-PERIODSLUT SECTION.                                        
043100     SKIP2                                                                
043200     IF TAB-TIRP (TIX) NOT = DAT-TIRP                                     
043300        MOVE REDO-PERIOD TO ORD                                           
043400                                                                          
043500        MOVE REDPER-PTR TO START-PTR                                      
043600        MOVE 5 TO IX-MAX                                                  
043700        PERFORM S10-ORD-MINUS-IX-DAGAR                                    
043800                                                                          
043900        PERFORM S07-LORDAGS-ORD                                           
044000                                                                          
044100        MOVE REDPERVECK-PTR TO START-PTR                                  
044200        MOVE 1 TO IX-MAX                                                  
044300        PERFORM S12-ORD-MINUS-IX-VECKOR                                   
044400                                                                          
044500        MOVE PVREDO-PERIOD TO ORD                                         
044600        MOVE PVREDO-PTR TO START-PTR                                      
044700        MOVE 1 TO IX-MAX                                                  
044800        PERFORM S10-ORD-MINUS-IX-DAGAR                                    
044900                                                                          
045000        PERFORM S07-LORDAGS-ORD                                           
045100     END-IF                                                               
045200     SKIP3                                                                
045300     SKIP3                                                                
045400     .                                                                    
045500 DH-TESTA-REDO-PERIODSTART SECTION.                                       
045600     SKIP2                                                                
045700     IF TAB-TIRP (TIX) NOT = DAT-TIRP                                     
045800        MOVE REDO-PERIOD  TO ORD                                          
045900                                                                          
046000        MOVE REDPER-PTR TO START-PTR                                      
046100        MOVE 5 TO IX-MAX                                                  
046200        PERFORM S11-ORD-PLUS-IX-DAGAR                                     
046300                                                                          
046400        MOVE REDPERVECK-PTR TO START-PTR                                  
046500        MOVE 3 TO IX-MAX                                                  
046600        PERFORM S13-ORD-PLUS-IX-VECKOR                                    
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 DI-TESTA-MANADSSLUT SECTION.                                             
047100     SKIP2                                                                
047200                                                                          
047300     IF TAB-TIMANAD (TIX) NOT = DAT-TIMM                                  
047400                                                                          
047500        MOVE MAN-PTR TO PTR                                               
047600        STRING MANAD DELIMITED BY SIZE                                    
047700        INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                           
047800        SUBTRACT 1 FROM TIX GIVING AIX                                    
047900                                                                          
048000        MOVE MANAD TO ORD                                                 
048100        MOVE MAN-PTR TO START-PTR                                         
048200        MOVE 5 TO IX-MAX                                                  
048300        PERFORM S10-ORD-MINUS-IX-DAGAR                                    
048400                                                                          
048500        MOVE TIX TO AIX                                                   
048600        MOVE MANVECK-PTR TO START-PTR                                     
048700        MOVE 2 TO IX-MAX                                                  
048800        PERFORM S12-ORD-MINUS-IX-VECKOR                                   
048900     END-IF                                                               
049000     .                                                                    
049100     SKIP3                                                                
049200     SKIP3                                                                
049300 DJ-TESTA-MANADSSTART SECTION.                                            
049400     SKIP2                                                                
049500     IF TAB-TIMANAD (TIX) NOT = DAT-TIMM                                  
049600        MOVE MANAD TO ORD                                                 
049700                                                                          
049800        MOVE MAN-PTR TO START-PTR                                         
049900        MOVE 5 TO IX-MAX                                                  
050000        PERFORM S11-ORD-PLUS-IX-DAGAR                                     
050100                                                                          
050200        MOVE MANVECK-PTR TO START-PTR                                     
050300        MOVE 2 TO IX-MAX                                                  
050400        PERFORM S13-ORD-PLUS-IX-VECKOR                                    
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800 DK-TESTA-ARSSLUT SECTION.                                                
050900     SKIP2                                                                
051000     IF TAB-TIAAR (TIX) NOT = DAT-TIAA                                    
051100         MOVE AAR TO ORD                                                  
051200                                                                          
051300         MOVE AAR-PTR TO START-PTR                                        
051400         MOVE 5 TO IX-MAX                                                 
051500         PERFORM S10-ORD-MINUS-IX-DAGAR                                   
051600                                                                          
051700         PERFORM S07-LORDAGS-ORD                                          
051800                                                                          
051900         MOVE AARVECK-PTR TO START-PTR                                    
052000         MOVE 5 TO IX-MAX                                                 
052100         PERFORM S12-ORD-MINUS-IX-VECKOR                                  
052200                                                                          
052300         MOVE AARMAN-PTR TO START-PTR                                     
052400         MOVE 5 TO IX-MAX                                                 
052500         PERFORM S14-ORD-MINUS-IX-MANADER                                 
052600                                                                          
052700         MOVE AARPLAN-PTR TO START-PTR                                    
052800         MOVE 3 TO IX-MAX                                                 
052900         PERFORM S16-ORD-MINUS-IX-PLANPER                                 
053000                                                                          
053100         MOVE AARREDO-PTR TO START-PTR                                    
053200         MOVE 5 TO IX-MAX                                                 
053300         PERFORM S18-ORD-MINUS-IX-REDOPER                                 
053400     END-IF                                                               
053500     .                                                                    
053600     SKIP3                                                                
053700     SKIP3                                                                
053800 DL-TESTA-ARSSTART  SECTION.                                              
053900     SKIP2                                                                
054000     IF TAB-TIAAR (TIX) NOT = DAT-TIAA                                    
054100         MOVE AAR TO ORD                                                  
054200                                                                          
054300         MOVE AAR-PTR TO START-PTR                                        
054400         MOVE 5 TO IX-MAX                                                 
054500         PERFORM S11-ORD-PLUS-IX-DAGAR                                    
054600                                                                          
054700         MOVE AARVECK-PTR TO START-PTR                                    
054800         MOVE 5 TO IX-MAX                                                 
054900         PERFORM S13-ORD-PLUS-IX-VECKOR                                   
055000                                                                          
055100         MOVE AARMAN-PTR TO START-PTR                                     
055200         MOVE 6 TO IX-MAX                                                 
055300         PERFORM S15-ORD-PLUS-IX-MANADER                                  
055400                                                                          
055500         MOVE AARPLAN-PTR TO START-PTR                                    
055600         MOVE 4 TO IX-MAX                                                 
055700         PERFORM S17-ORD-PLUS-IX-PLANPER                                  
055800                                                                          
055900         MOVE AARREDO-PTR TO START-PTR                                    
056000         MOVE 6 TO IX-MAX                                                 
056100         PERFORM S19-ORD-PLUS-IX-REDOPER                                  
056200     END-IF                                                               
056300     .                                                                    
056400     EJECT                                                                
056500 DM-TESTA-PV-PLAN-PERIODSLUT  SECTION.                                    
056600     SKIP2                                                                
056700     IF TAB-TIPP  (TIX) NOT = DAT-TIPP                                    
056800                                                                          
056900         MOVE PVPLAN-PERIOD TO ORD                                        
057000         MOVE PVPLAN-PTR TO START-PTR                                     
057100         MOVE 1 TO IX-MAX                                                 
057200         PERFORM S10-ORD-MINUS-IX-DAGAR                                   
057300                                                                          
057400         PERFORM S07-LORDAGS-ORD                                          
057500                                                                          
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057900 DN-TESTA-MANADSNAMN SECTION.                                             
058000     SKIP2                                                                
058100     EVALUATE TAB-TIMM (TIX)                                              
058200       WHEN 1    MOVE 'JAN' TO MANADSNAMN                                 
058300       WHEN 2    MOVE 'FEB' TO MANADSNAMN                                 
058400       WHEN 3    MOVE 'MAR' TO MANADSNAMN                                 
058500       WHEN 4    MOVE 'APR' TO MANADSNAMN                                 
058600       WHEN 5    MOVE 'MAY' TO MANADSNAMN                                 
058700       WHEN 6    MOVE 'JUN' TO MANADSNAMN                                 
058800       WHEN 7    MOVE 'JUL' TO MANADSNAMN                                 
058900       WHEN 8    MOVE 'AUG' TO MANADSNAMN                                 
059000       WHEN 9    MOVE 'SEP' TO MANADSNAMN                                 
059100       WHEN 10   MOVE 'OCT' TO MANADSNAMN                                 
059200       WHEN 11   MOVE 'NOV' TO MANADSNAMN                                 
059300       WHEN 12   MOVE 'DEC' TO MANADSNAMN                                 
059400     END-EVALUATE                                                         
059500     SKIP2                                                                
059600     MOVE MANADSNAMN-PTR TO PTR                                           
059700     STRING MANADSNAMN DELIMITED BY SIZE                                  
059800     INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                              
059900     .                                                                    
060000     EJECT                                                                
060100 DO-GENERERA-DATUMET SECTION.                                             
060200     SKIP2                                                                
060300     MOVE DATUM-PTR TO PTR                                                
060400     STRING TAB-TIMMDD (TIX) DELIMITED BY SIZE                            
060500     INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                              
060600     .                                                                    
060700     EJECT                                                                
060800 DP-TESTA-VECKODAG-MANADSSLUT   SECTION.                                  
060900     SKIP2                                                                
061000     IF TAB-TIMANAD (TIX) NOT = DAT-TIMM                                  
061100       MOVE MANAD TO ORD                                                  
061200                                                                          
061300       MOVE 2 TO IX-MAX                                                   
061400       MOVE MANVECKDAG-PTR TO START-PTR                                   
061500       PERFORM S08-ORD-MINUS-VECKDAG                                      
061600     END-IF                                                               
061700     .                                                                    
061800     SKIP3                                                                
061900     SKIP3                                                                
062000 DQ-TESTA-VECKODAG-MANADSSTART  SECTION.                                  
062100     SKIP2                                                                
062200     IF TAB-TIMANAD (TIX) NOT = DAT-TIMM                                  
062300       MOVE MANAD TO ORD                                                  
062400                                                                          
062500       MOVE 2 TO IX-MAX                                                   
062600       MOVE MANVECKDAG-PTR TO START-PTR                                   
062700       PERFORM S09-ORD-PLUS-VECKDAG                                       
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 DR-UDDA-JAMN-VECKA SECTION.                                              
063200     SKIP2                                                                
063300     IF FUNCTION MOD (TAB-TIVV (TIX), 2) = 0                              
063400       MOVE JAMN-VECKA TO ORD                                             
063500     ELSE                                                                 
063600       MOVE UDDA-VECKA TO ORD                                             
063700     END-IF                                                               
063800                                                                          
063900     MOVE UJ-VECKA-PTR TO PTR                                             
064000     STRING ORD DELIMITED BY SIZE                                         
064100     INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                              
064200     .                                                                    
064300     EJECT                                                                
064400 DS-TESTA-KVARTALSSLUT SECTION.                                           
064500     SKIP2                                                                
064600     IF TAB-TIRP (TIX) NOT = DAT-TIRP                                     
064700     AND (TAB-TIRP (TIX) = 3 OR 6 OR 9 OR 12)                             
064800        MOVE KVARTAL     TO ORD                                           
064900                                                                          
065000        MOVE KVARTAL-PTR TO START-PTR                                     
065100        MOVE 5 TO IX-MAX                                                  
065200        PERFORM S10-ORD-MINUS-IX-DAGAR                                    
065300                                                                          
065400        PERFORM S07-LORDAGS-ORD                                           
065500                                                                          
065600        MOVE KVARTALVECK-PTR TO START-PTR                                 
065700        MOVE 3 TO IX-MAX                                                  
065800        PERFORM S12-ORD-MINUS-IX-VECKOR                                   
065900                                                                          
066000     END-IF                                                               
066100     SKIP3                                                                
066200     SKIP3                                                                
066300     .                                                                    
066400 DT-TESTA-KVARTALSSTART SECTION.                                          
066500     SKIP2                                                                
066600     IF TAB-TIRP (TIX) NOT = DAT-TIRP                                     
066700     AND (TAB-TIRP (TIX) = 1 OR 4 OR 7 OR 10)                             
066800        MOVE KVARTAL      TO ORD                                          
066900                                                                          
067000        MOVE KVARTAL-PTR TO START-PTR                                     
067100        MOVE 5 TO IX-MAX                                                  
067200        PERFORM S11-ORD-PLUS-IX-DAGAR                                     
067300                                                                          
067400        MOVE KVARTALVECK-PTR TO START-PTR                                 
067500        MOVE 3 TO IX-MAX                                                  
067600        PERFORM S13-ORD-PLUS-IX-VECKOR                                    
067700     END-IF                                                               
067800     .                                                                    
067900 S01-ANROPA-WDAGKONV SECTION.                                             
068000     SKIP2                                                                
068100     CALL WDAGKONV USING                                                  
068200         DAG-KDCALL                                                       
068300         DAG-DATUM-AREA                                                   
068400         DAG-KDSVAR                                                       
068500     IF DAG-KDSVAR NOT = SPACE                                            
068600         DISPLAY 'FEL I ANROP TILL WDAGKONV '                             
068700                 'START: ' STARTA ' STOPP: ' STOPPA                       
068800         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
068900     END-IF                                                               
069000     SKIP3                                                                
069100     .                                                                    
069200 S02-ANROPA-WDATKONV SECTION.                                             
069300     SKIP2                                                                
069400     CALL WDATKONV USING                                                  
069500         DAT-KDDATFORM                                                    
069600         DAT-I-TIDATUM                                                    
069700         DAT-O-TIDATUM                                                    
069800         DAT-KDSVAR                                                       
069900     IF DAT-KDSVAR-FEL                                                    
070000         DISPLAY 'FEL I ANROP TILL WDATKONV '                             
070100                 'DATUM: ' DAT-I-TIDATUM                                  
070200                 'DATAREA: ' DAT-WDATAREA                                 
070300         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
070400     END-IF                                                               
070500     .                                                                    
070600     EJECT                                                                
070700 S03-OEKA-KOLLA-DATUM SECTION.                                            
070800     SKIP2                                                                
070900     EVALUATE TRUE                                                        
071000       WHEN  W-MANAD = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12             
071100         MOVE 31 TO W-SISTA-I-MANAD                                       
071200       WHEN  W-MANAD = 04 OR 06 OR 09 OR 11                               
071300         MOVE 30 TO W-SISTA-I-MANAD                                       
071400       WHEN  W-MANAD = 02                                                 
071500         IF FUNCTION MOD (W-AAR, 4) = 0                                   
071600             MOVE 29 TO W-SISTA-I-MANAD                                   
071700         ELSE                                                             
071800             MOVE 28 TO W-SISTA-I-MANAD                                   
071900         END-IF                                                           
072000     END-EVALUATE                                                         
072100                                                                          
072200     ADD 1 TO W-DAG                                                       
072300                                                                          
072400     IF W-DAG > W-SISTA-I-MANAD                                           
072500         ADD 1 TO W-MANAD                                                 
072600         MOVE 01 TO W-DAG                                                 
072700     END-IF                                                               
072800     IF W-MANAD > 12                                                      
072900         IF W-AAR < 99                                                    
073000           ADD 1 TO W-AAR                                                 
073100         ELSE                                                             
073200           MOVE 00 TO W-AAR                                               
073300         END-IF                                                           
073400         MOVE 01 TO W-MANAD                                               
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800 S04-SKRIV-HELA-CALENDAR SECTION.                                         
073900     SKIP2                                                                
074000     MOVE 1 TO TIX                                                        
074100     PERFORM UNTIL TAB-TIDATUM (TIX) = SPAR-DATUM-FOM                     
074200         ADD 1 TO TIX                                                     
074300     END-PERFORM                                                          
074400     MOVE TAB-TIDATUM (TIX)   TO TMP1-YYMMDD                              
074500     MOVE SPAR-DATUM-TOM      TO TMP2-YYMMDD                              
074600     PERFORM WY2000P1                                                     
074700     PERFORM UNTIL TIX > TIX-MAX OR TMP1-YYMMDD > TMP2-YYMMDD             
074800                                                                          
074900         MOVE KVARTAL-PTR TO PTR                                          
075000         MOVE SPACE TO WORD-1 WORD-2 WORD-3 WORD-4                        
075100                       WORD-5 WORD-6 WORD-7 WORD-8                        
075200                       WORD-9 WORD-10 WORD-11 WORD-12                     
075300         UNSTRING TAB-KAT-ORD (TIX) DELIMITED BY ALL SPACE                
075400         INTO WORD-1 WORD-2 WORD-3 WORD-4 WORD-5 WORD-6                   
075500              WORD-7 WORD-8 WORD-9 WORD-10 WORD-11 WORD-12                
075600         WITH POINTER PTR                                                 
075700         END-UNSTRING                                                     
075800                                                                          
075900         MOVE KVARTAL-PTR TO PTR                                          
076000         STRING WORD-1 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076100                WORD-2 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076200                WORD-3 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076300                WORD-4 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076400                WORD-5 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076500                WORD-6 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076600                WORD-7 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076700                WORD-8 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076800                WORD-9 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
076900               WORD-10 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
077000               WORD-11 DELIMITED BY SPACE '  ' DELIMITED BY SIZE          
077100               WORD-12 DELIMITED BY SPACE                                 
077200         INTO TAB-KAT-ORD (TIX) WITH POINTER PTR                          
077300         MOVE SPACE TO TAB-KAT-ORD (TIX) (PTR:)                           
077400         WRITE CALENDAR-POST FROM TAB-UT-RAD (TIX)                        
077500         ADD 1 TO TIX                                                     
077600         IF TIX <= TIX-MAX                                                
077700           MOVE TAB-TIDATUM (TIX)   TO TMP1-YYMMDD                        
077800           MOVE SPAR-DATUM-TOM      TO TMP2-YYMMDD                        
077900           PERFORM WY2000P1                                               
078000         END-IF                                                           
078100     END-PERFORM                                                          
078200     .                                                                    
078300     EJECT                                                                
078400 S07-LORDAGS-ORD SECTION.                                                 
078500                                                                          
078600*    -- LÄGG ETT KALENDERORD PÅ EFTERFÖLJANDE LÖRDAG                      
078700*    -- UTOM VID ÅRSSLUT                                                  
078800                                                                          
078900     ADD TIX 1 GIVING AIX                                                 
079000     PERFORM UNTIL TAB-TID (AIX) = 6                                      
079100       OR TAB-TIMMDD (AIX) = '0101'                                       
079200       ADD 1 TO AIX                                                       
079300     END-PERFORM                                                          
079400                                                                          
079500     MOVE START-PTR TO PTR                                                
079600     STRING ORD   DELIMITED BY SPACE                                      
079700       INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                            
079800     .                                                                    
079900     EJECT                                                                
080000 S08-ORD-MINUS-VECKDAG  SECTION.                                          
080100                                                                          
080200     MOVE TIX TO AIX                                                      
080300     MOVE 1 TO IX                                                         
080400     PERFORM UNTIL AIX = 0 OR IX > IX-MAX                                 
080500                                                                          
080600         IF IX > 1                                                        
080700           MOVE IX TO IX-ZONAT                                            
080800         ELSE                                                             
080900           MOVE SPACE TO IX-ZONAT-X                                       
081000         END-IF                                                           
081100                                                                          
081200         MOVE 1 TO BIX                                                    
081300         PERFORM UNTIL AIX = 0 OR BIX > 7                                 
081400           MOVE TAB-TID (AIX) TO TID                                      
081500           MOVE START-PTR TO PTR                                          
081600           STRING ORD   DELIMITED BY SPACE                                
081700                  MIN DELIMITED BY SIZE                                   
081800                  IX-ZONAT-X DELIMITED BY SPACE                           
081900                  VECKDAG (TID) DELIMITED BY SIZE                         
082000             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
082100                                                                          
082200           ADD 1 TO BIX                                                   
082300           SUBTRACT 1 FROM AIX                                            
082400         END-PERFORM                                                      
082500                                                                          
082600         ADD 1 TO IX                                                      
082700     END-PERFORM                                                          
082800     .                                                                    
082900     EJECT                                                                
083000 S09-ORD-PLUS-VECKDAG  SECTION.                                           
083100                                                                          
083200     MOVE TIX TO AIX                                                      
083300     MOVE 1 TO IX                                                         
083400     PERFORM UNTIL AIX > TIX-MAX  OR  IX > IX-MAX                         
083500                                                                          
083600         IF IX > 1                                                        
083700           MOVE IX TO IX-ZONAT                                            
083800         ELSE                                                             
083900           MOVE SPACE TO IX-ZONAT-X                                       
084000         END-IF                                                           
084100                                                                          
084200         MOVE 1 TO BIX                                                    
084300         PERFORM UNTIL AIX > TIX-MAX  OR  BIX > 7                         
084400           MOVE TAB-TID (AIX) TO TID                                      
084500           MOVE START-PTR TO PTR                                          
084600           STRING ORD   DELIMITED BY SPACE                                
084700                  PLU DELIMITED BY SIZE                                   
084800                  IX-ZONAT-X DELIMITED BY SPACE                           
084900                  VECKDAG (TID) DELIMITED BY SIZE                         
085000             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
085100                                                                          
085200           ADD 1 TO AIX                                                   
085300           ADD 1 TO BIX                                                   
085400         END-PERFORM                                                      
085500                                                                          
085600         ADD 1 TO IX                                                      
085700     END-PERFORM                                                          
085800     .                                                                    
085900     EJECT                                                                
086000 S10-ORD-MINUS-IX-DAGAR SECTION.                                          
086100                                                                          
086200*    -- VID MÅNADS/ÅRS-SLUT SKA ORDET UTAN MINUS OCH DAGNR                
086300*    -- HAMNA PÅ SISTA DAGEN OCH ORD MINUS ETT SKA HAMNA PÅ               
086400*    -- FÖREGÅENDE DAG. VID MÅNDASSLUT ÄR DÄRFÖR AIX REDAN                
086500*    -- SATT AV ANROPANDE SEKTION.                                        
086600     IF ORD NOT = MANAD AND AAR                                           
086700       MOVE TIX TO AIX                                                    
086800     END-IF                                                               
086900                                                                          
087000     MOVE 1 TO IX                                                         
087100     PERFORM UNTIL AIX = 0  OR  IX > IX-MAX                               
087200                                                                          
087300         IF TAB-KVARBDAG (AIX) = 1                                        
087400             MOVE IX TO IX-ZONAT                                          
087500             MOVE START-PTR TO PTR                                        
087600             STRING ORD   DELIMITED BY SPACE                              
087700                    MIN DELIMITED BY SIZE                                 
087800                    IX-ZONAT DELIMITED BY SIZE                            
087900                    ' ' DELIMITED BY SIZE                                 
088000               INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                    
088100                                                                          
088200             ADD 1 TO IX                                                  
088300         END-IF                                                           
088400                                                                          
088500         SUBTRACT 1 FROM AIX                                              
088600     END-PERFORM                                                          
088700     .                                                                    
088800     EJECT                                                                
088900 S11-ORD-PLUS-IX-DAGAR SECTION.                                           
089000                                                                          
089100     MOVE TIX TO AIX                                                      
089200     MOVE 1 TO IX                                                         
089300     PERFORM UNTIL AIX > TIX-MAX  OR IX > IX-MAX                          
089400                                                                          
089500         IF TAB-KVARBDAG (AIX) = 1                                        
089600             MOVE IX TO IX-ZONAT                                          
089700             MOVE START-PTR TO PTR                                        
089800             STRING ORD   DELIMITED BY SPACE                              
089900                    IX-ZONAT DELIMITED BY SIZE                            
090000                    ' ' DELIMITED BY SIZE                                 
090100               INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                    
090200                                                                          
090300             ADD 1 TO IX                                                  
090400         END-IF                                                           
090500                                                                          
090600         ADD 1 TO AIX                                                     
090700     END-PERFORM                                                          
090800     .                                                                    
090900     EJECT                                                                
091000 S12-ORD-MINUS-IX-VECKOR    SECTION.                                      
091100                                                                          
091200*                     TIX STÅR PÅ SISTA ARBETSDAGEN I EN                  
091300*                     TIDSPERIOD. TA FÖREGÅENDE LÖRDAGAR                  
091400                                                                          
091500     MOVE 1 TO IX                                                         
091600     MOVE AIX TO BIX                                                      
091700     PERFORM UNTIL AIX = 0  OR  IX > IX-MAX                               
091800                                                                          
091900       IF TAB-TID (AIX) = 6                                               
092000          AND TAB-TIVV (AIX) NOT = TAB-TIVV (BIX)                         
092100                                                                          
092200           IF IX > 1                                                      
092300             MOVE IX TO IX-ZONAT                                          
092400           ELSE                                                           
092500             MOVE SPACE TO IX-ZONAT-X                                     
092600           END-IF                                                         
092700                                                                          
092800           MOVE START-PTR TO PTR                                          
092900           STRING ORD  DELIMITED BY SPACE                                 
093000                  MIN DELIMITED BY SIZE                                   
093100                  IX-ZONAT-X DELIMITED BY SPACE                           
093200                  VECKA DELIMITED BY SIZE                                 
093300                  ' ' DELIMITED BY SIZE                                   
093400             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
093500                                                                          
093600           MOVE AIX TO BIX                                                
093700           ADD 1 TO IX                                                    
093800                                                                          
093900       END-IF                                                             
094000                                                                          
094100       SUBTRACT 1 FROM AIX                                                
094200     END-PERFORM                                                          
094300     .                                                                    
094400     EJECT                                                                
094500 S13-ORD-PLUS-IX-VECKOR    SECTION.                                       
094600                                                                          
094700*           -- TIX STÅR PÅ 1:A ARBETSDAGEN I EN TIDSPERIOD                
094800                                                                          
094900     MOVE TIX TO AIX                                                      
095000     MOVE 1 TO IX                                                         
095100     PERFORM UNTIL AIX > TIX-MAX  OR  IX > IX-MAX                         
095200                                                                          
095300*           -- HITTA EFTERFÖLJANDE LÖRDAG                                 
095400       ADD 1 AIX GIVING BIX                                               
095500       PERFORM UNTIL BIX > TIX-MAX  OR  TAB-TID (BIX) = 6                 
095600          ADD 1 TO BIX                                                    
095700       END-PERFORM                                                        
095800                                                                          
095900       IF BIX <= TIX-MAX                                                  
096000       AND TAB-TIVV (AIX) NOT = TAB-TIVV (BIX)                            
096100                                                                          
096200           IF IX > 1                                                      
096300             MOVE IX TO IX-ZONAT                                          
096400           ELSE                                                           
096500             MOVE SPACE TO IX-ZONAT-X                                     
096600           END-IF                                                         
096700                                                                          
096800           MOVE START-PTR TO PTR                                          
096900           STRING ORD  DELIMITED BY SPACE                                 
097000                  PLU  DELIMITED BY SIZE                                  
097100                  IX-ZONAT-X DELIMITED BY SPACE                           
097200                  VECKA DELIMITED BY SIZE                                 
097300                  ' ' DELIMITED BY SIZE                                   
097400             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
097500                                                                          
097600           ADD 1 TO IX                                                    
097700       END-IF                                                             
097800                                                                          
097900       MOVE BIX TO AIX                                                    
098000     END-PERFORM                                                          
098100     .                                                                    
098200     EJECT                                                                
098300 S14-ORD-MINUS-IX-MANADER  SECTION.                                       
098400                                                                          
098500*                     TIX STÅR PÅ SISTA ARBETSDAGEN I EN                  
098600*                     TIDSPERIOD.                                         
098700                                                                          
098800     MOVE 1 TO IX                                                         
098900     MOVE TIX TO AIX BIX                                                  
099000     PERFORM UNTIL AIX = 0  OR IX > IX-MAX                                
099100                                                                          
099200       IF TAB-TID (AIX) <= 6                                              
099300          AND TAB-TIMM (AIX) NOT = TAB-TIMM (BIX)                         
099400                                                                          
099500           IF IX > 1                                                      
099600             MOVE IX TO IX-ZONAT                                          
099700           ELSE                                                           
099800             MOVE SPACE TO IX-ZONAT-X                                     
099900           END-IF                                                         
100000                                                                          
100100           MOVE START-PTR TO PTR                                          
100200           STRING ORD  DELIMITED BY SPACE                                 
100300                  MIN DELIMITED BY SIZE                                   
100400                  IX-ZONAT-X DELIMITED BY SPACE                           
100500                  MANAD DELIMITED BY SIZE                                 
100600                  ' ' DELIMITED BY SIZE                                   
100700             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
100800                                                                          
100900           MOVE AIX TO BIX                                                
101000           ADD 1 TO IX                                                    
101100                                                                          
101200       END-IF                                                             
101300                                                                          
101400       SUBTRACT 1 FROM AIX                                                
101500     END-PERFORM                                                          
101600     .                                                                    
101700     EJECT                                                                
101800 S15-ORD-PLUS-IX-MANADER  SECTION.                                        
101900                                                                          
102000*              TIX STÅR PÅ 1:A ARBETSDAGEN I EN TIDSPERIOD                
102100                                                                          
102200     MOVE TIX TO AIX                                                      
102300     MOVE 1 TO IX                                                         
102400     PERFORM UNTIL AIX > TIX-MAX  OR  IX > IX-MAX                         
102500                                                                          
102600       ADD 1 AIX GIVING BIX                                               
102700       PERFORM UNTIL BIX > TIX-MAX  OR  TAB-TID (BIX) <= 6                
102800          ADD 1 TO BIX                                                    
102900       END-PERFORM                                                        
103000                                                                          
103100       IF BIX <= TIX-MAX                                                  
103200       AND TAB-TIMM (AIX) NOT = TAB-TIMM (BIX)                            
103300                                                                          
103400           IF IX > 1                                                      
103500             MOVE IX TO IX-ZONAT                                          
103600           ELSE                                                           
103700             MOVE SPACE TO IX-ZONAT-X                                     
103800           END-IF                                                         
103900                                                                          
104000           MOVE START-PTR TO PTR                                          
104100           STRING ORD  DELIMITED BY SPACE                                 
104200                  PLU  DELIMITED BY SIZE                                  
104300                  IX-ZONAT-X DELIMITED BY SPACE                           
104400                  MANAD DELIMITED BY SIZE                                 
104500                  ' ' DELIMITED BY SIZE                                   
104600             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
104700                                                                          
104800           ADD 1 TO IX                                                    
104900       END-IF                                                             
105000                                                                          
105100       MOVE BIX TO AIX                                                    
105200     END-PERFORM                                                          
105300     .                                                                    
105400     EJECT                                                                
105500 S16-ORD-MINUS-IX-PLANPER  SECTION.                                       
105600                                                                          
105700*                     TIX STÅR PÅ SISTA ARBETSDAGEN I EN                  
105800*                     TIDSPERIOD. TA FÖREGÅENDE LÖRDAGAR                  
105900                                                                          
106000     MOVE 1 TO IX                                                         
106100     MOVE TIX TO AIX BIX                                                  
106200     PERFORM  UNTIL AIX = 0  OR  IX > IX-MAX                              
106300                                                                          
106400       IF TAB-TID (AIX) = 6                                               
106500          AND TAB-TIP (AIX) NOT = TAB-TIP (BIX)                           
106600                                                                          
106700           IF IX > 1                                                      
106800             MOVE IX TO IX-ZONAT                                          
106900           ELSE                                                           
107000             MOVE SPACE TO IX-ZONAT-X                                     
107100           END-IF                                                         
107200                                                                          
107300           MOVE START-PTR TO PTR                                          
107400           STRING ORD  DELIMITED BY SPACE                                 
107500                  MIN DELIMITED BY SIZE                                   
107600                  IX-ZONAT-X DELIMITED BY SPACE                           
107700                  PLAN-PERIOD DELIMITED BY SIZE                           
107800                  ' ' DELIMITED BY SIZE                                   
107900             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
108000                                                                          
108100           MOVE AIX TO BIX                                                
108200           ADD 1 TO IX                                                    
108300                                                                          
108400       END-IF                                                             
108500                                                                          
108600       SUBTRACT 1 FROM AIX                                                
108700     END-PERFORM                                                          
108800     .                                                                    
108900     EJECT                                                                
109000 S17-ORD-PLUS-IX-PLANPER  SECTION.                                        
109100                                                                          
109200*           -- TIX STÅR PÅ 1:A ARBETSDAGEN I EN TIDSPERIOD                
109300                                                                          
109400     MOVE TIX TO AIX                                                      
109500     MOVE 1 TO IX                                                         
109600     PERFORM UNTIL AIX > TIX-MAX  OR  IX > IX-MAX                         
109700                                                                          
109800*           -- HITTA EFTERFÖLJANDE LÖRDAG                                 
109900       ADD 1 AIX GIVING BIX                                               
110000       PERFORM UNTIL BIX > TIX-MAX  OR  TAB-TID (BIX) = 6                 
110100          ADD 1 TO BIX                                                    
110200       END-PERFORM                                                        
110300                                                                          
110400       IF BIX <= TIX-MAX                                                  
110500       AND TAB-TIP  (AIX) NOT = TAB-TIP  (BIX)                            
110600                                                                          
110700           IF IX > 1                                                      
110800             MOVE IX TO IX-ZONAT                                          
110900           ELSE                                                           
111000             MOVE SPACE TO IX-ZONAT-X                                     
111100           END-IF                                                         
111200                                                                          
111300           MOVE START-PTR TO PTR                                          
111400           STRING ORD  DELIMITED BY SPACE                                 
111500                  PLU  DELIMITED BY SIZE                                  
111600                  IX-ZONAT-X DELIMITED BY SPACE                           
111700                  PLAN-PERIOD DELIMITED BY SIZE                           
111800                  ' ' DELIMITED BY SIZE                                   
111900             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
112000                                                                          
112100           ADD 1 TO IX                                                    
112200       END-IF                                                             
112300                                                                          
112400       MOVE BIX TO AIX                                                    
112500     END-PERFORM                                                          
112600     .                                                                    
112700     EJECT                                                                
112800 S18-ORD-MINUS-IX-REDOPER  SECTION.                                       
112900                                                                          
113000*                     TIX STÅR PÅ SISTA ARBETSDAGEN I EN                  
113100*                     TIDSPERIOD. TA FÖREGÅENDE LÖRDAGAR                  
113200                                                                          
113300     MOVE 1 TO IX                                                         
113400     MOVE TIX TO AIX BIX                                                  
113500     PERFORM UNTIL AIX = 0  OR  IX > IX-MAX                               
113600                                                                          
113700       IF TAB-TID (AIX) = 6                                               
113800          AND TAB-TIRP  (AIX) NOT = TAB-TIRP  (BIX)                       
113900                                                                          
114000           IF IX > 1                                                      
114100             MOVE IX TO IX-ZONAT                                          
114200           ELSE                                                           
114300             MOVE SPACE TO IX-ZONAT-X                                     
114400           END-IF                                                         
114500                                                                          
114600           MOVE START-PTR TO PTR                                          
114700           STRING ORD  DELIMITED BY SPACE                                 
114800                  MIN DELIMITED BY SIZE                                   
114900                  IX-ZONAT-X DELIMITED BY SPACE                           
115000                  REDO-PERIOD DELIMITED BY SIZE                           
115100                  ' ' DELIMITED BY SIZE                                   
115200             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
115300                                                                          
115400           MOVE AIX TO BIX                                                
115500           ADD 1 TO IX                                                    
115600                                                                          
115700       END-IF                                                             
115800                                                                          
115900       SUBTRACT 1 FROM AIX                                                
116000     END-PERFORM                                                          
116100     .                                                                    
116200     EJECT                                                                
116300 S19-ORD-PLUS-IX-REDOPER  SECTION.                                        
116400                                                                          
116500*           -- TIX STÅR PÅ 1:A ARBETSDAGEN I EN TIDSPERIOD                
116600                                                                          
116700     MOVE TIX TO AIX                                                      
116800     MOVE 1 TO IX                                                         
116900     PERFORM UNTIL AIX > TIX-MAX  OR  IX > IX-MAX                         
117000                                                                          
117100*           -- HITTA EFTERFÖLJANDE LÖRDAG                                 
117200       ADD 1 AIX GIVING BIX                                               
117300       PERFORM UNTIL  BIX > TIX-MAX  OR  TAB-TID (BIX) = 6                
117400          ADD 1 TO BIX                                                    
117500       END-PERFORM                                                        
117600                                                                          
117700       IF BIX <= TIX-MAX                                                  
117800       AND TAB-TIRP  (AIX) NOT = TAB-TIRP  (BIX)                          
117900                                                                          
118000           IF IX > 1                                                      
118100             MOVE IX TO IX-ZONAT                                          
118200           ELSE                                                           
118300             MOVE SPACE TO IX-ZONAT-X                                     
118400           END-IF                                                         
118500                                                                          
118600           MOVE START-PTR TO PTR                                          
118700           STRING ORD  DELIMITED BY SPACE                                 
118800                  PLU  DELIMITED BY SIZE                                  
118900                  IX-ZONAT-X DELIMITED BY SPACE                           
119000                  REDO-PERIOD DELIMITED BY SIZE                           
119100                  ' ' DELIMITED BY SIZE                                   
119200             INTO TAB-KAT-ORD (AIX) WITH POINTER PTR                      
119300                                                                          
119400           ADD 1 TO IX                                                    
119500       END-IF                                                             
119600                                                                          
119700       MOVE BIX TO AIX                                                    
119800     END-PERFORM                                                          
119900     .                                                                    
120000     EJECT                                                                
120100 S20-NAESTA-ARBDAG  SECTION.                                              
120200                                                                          
120300     ADD 1 TIX GIVING BIX                                                 
120400     IF BIX <= TIX-MAX                                                    
120500       PERFORM UNTIL BIX = TIX-MAX  OR  TAB-KVARBDAG (BIX) = 1            
120600          ADD 1 TO BIX                                                    
120700       END-PERFORM                                                        
120800                                                                          
120900       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
121000       MOVE TAB-TIDATUM (BIX) TO DAT-I-TIDATUM                            
121100       PERFORM S02-ANROPA-WDATKONV                                        
121200     END-IF                                                               
121300     SKIP3                                                                
121400     SKIP3                                                                
121500     .                                                                    
121600 S21-FOREG-ARBDAG SECTION.                                                
121700                                                                          
121800     SUBTRACT 1 FROM TIX GIVING BIX                                       
121900     IF BIX >= 1                                                          
122000       PERFORM UNTIL BIX = 1  OR  TAB-KVARBDAG (BIX) = 1                  
122100          SUBTRACT 1 FROM BIX                                             
122200       END-PERFORM                                                        
122300                                                                          
122400       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
122500       MOVE TAB-TIDATUM (BIX) TO DAT-I-TIDATUM                            
122600       PERFORM S02-ANROPA-WDATKONV                                        
122700     END-IF                                                               
122800     .                                                                    
122900     EJECT                                                                
123000 S22-NAESTA-DAG  SECTION.                                                 
123100                                                                          
123200     ADD 1 TIX GIVING BIX                                                 
123300                                                                          
123400     IF BIX <= TIX-MAX                                                    
123500       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
123600       MOVE TAB-TIDATUM (BIX) TO DAT-I-TIDATUM                            
123700       PERFORM S02-ANROPA-WDATKONV                                        
123800     END-IF                                                               
123900     SKIP3                                                                
124000     SKIP3                                                                
124100     .                                                                    
124200 S23-FOREG-DAG SECTION.                                                   
124300                                                                          
124400     SUBTRACT 1 FROM TIX GIVING BIX                                       
124500                                                                          
124600     IF BIX > 0                                                           
124700       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
124800       MOVE TAB-TIDATUM (BIX) TO DAT-I-TIDATUM                            
124900       PERFORM S02-ANROPA-WDATKONV                                        
125000     END-IF                                                               
125100     .                                                                    
125200     EJECT                                                                
125300 S25-NAESTA-ICKE-SONDAG SECTION.                                          
125400                                                                          
125500     ADD 1 TIX GIVING BIX                                                 
125600     IF BIX <= TIX-MAX                                                    
125700       PERFORM UNTIL BIX = TIX-MAX  OR  TAB-TID (BIX) NOT = 7             
125800          ADD 1 TO BIX                                                    
125900       END-PERFORM                                                        
126000                                                                          
126100       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
126200       MOVE TAB-TIDATUM (BIX) TO DAT-I-TIDATUM                            
126300       PERFORM S02-ANROPA-WDATKONV                                        
126400     END-IF                                                               
126500     SKIP3                                                                
126600     SKIP3                                                                
126700     .                                                                    
126800 S26-FOREG-ICKE-SONDAG SECTION.                                           
126900                                                                          
127000     SUBTRACT 1 FROM TIX GIVING BIX                                       
127100     IF BIX >= 1                                                          
127200       PERFORM UNTIL BIX = 1  OR  TAB-TID (BIX) NOT = 7                   
127300          SUBTRACT 1 FROM BIX                                             
127400       END-PERFORM                                                        
127500                                                                          
127600       MOVE 'AAMMDD' TO DAT-KDDATFORM                                     
127700       MOVE TAB-TIDATUM (BIX) TO DAT-I-TIDATUM                            
127800       PERFORM S02-ANROPA-WDATKONV                                        
127900     END-IF                                                               
128000     .                                                                    
128100     EJECT                                                                
128200*    -COPY WY2000P1                                                       
