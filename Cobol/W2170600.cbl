000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2170600.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   01/11/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SB SOM LÄSER UT KVOI-INFO                                        
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDL8                                       
001100*                                                                         
001110* OBS OM JOBBET KÖRS FÄRDIGT EFTER PERIODSKIFTE SKA DATUM                 
001120* HÅRDKODAS I A-INIT.                                                     
001130*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001510*    ÄNDRINGAR:                                                           
001511*       2005-12-16                                                        
001520*       VÄSENTLIG OMSKRIVNING AV HUVUDSLINGAN FÖR ATT UNDVIKA             
001521*       FEL OUTPUT NÄR PERIOD 11 KÖRS I DECEMBER.                         
001530*                                                                         
001540*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- KVOI-INFO                                                  
002500     SELECT W21706                     ASSIGN TO W21706D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W21706                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  POST -COPY W2170601 -PRE  UT-  -L.                                   
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W2170600'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  IX                          PIC 9(2).                                
004201 77  PERIOD-STATUS-WS            PIC X       VALUE 'N'.                   
004202     88  SISTA-PERIODEN                      VALUE 'J'.                   
004203     88  VANLIG-PERIOD                       VALUE 'N'.                   
004210                                                                          
004300 01  OI-ARTAL-0                  PIC 9(4).                                
004400 01  OI-ARTAL-1                  PIC 9(4).                                
004500 01  OI-ARTAL-2                  PIC 9(4).                                
004600 01  OI-ARTAL-3                  PIC 9(4).                                
004700 01  OI-ARTAL-4                  PIC 9(4).                                
004800 01  OI-ARTAL-5                  PIC 9(4).                                
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(4).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005501                                                                          
005510 01  WS-OVRIGA-DATUM-FALT.                                                
005521     03  WS-AAVV-RULL-FROM       PIC 9(4).                                
005522                                                                          
005523     03  WS-RULL-FROM  REDEFINES WS-AAVV-RULL-FROM.                       
005524        05  WS-AA-RULL-FROM      PIC 99.                                  
005525        05  WS-VV-RULL-FROM      PIC 99.                                  
005526                                                                          
005527     03  WS-AAVV-RULL-TO         PIC 9(4).                                
005528                                                                          
005529     03  WS-RULL-TO  REDEFINES WS-AAVV-RULL-TO.                           
005530        05  WS-AA-RULL-TO        PIC 99.                                  
005531        05  WS-VV-RULL-TO        PIC 99.                                  
005540                                                                          
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006200     SKIP2                                                                
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
007200*01 -COPY WDATAREA                                                        
007300     SKIP3                                                                
007400 01  UT-AREA-START               PIC X(24)   VALUE                        
007500                                 'UT-AREA-START  '.                       
007600     SKIP2                                                                
007700                                                                          
007800*01  AREA -COPY W2170601     -PRE UT-                                     
007900     EJECT                                                                
008000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008100*                                                                         
008200     EJECT                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL8'.                        
009900 01  DLI-IO-WDL8    PIC X(2000).                                          
010000*01  WDL801      -COPY WDL801               -RED DLI-IO-WDL8.             
010100     EJECT                                                                
010200*01  WDL811      -COPY WDL811               -RED DLI-IO-WDL8.             
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500*01  -COPY W0008  -PRE WDL8-                                              
010600     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010800 PROCEDURE DIVISION  USING WDL8-PCB.                                      
010900 MAIN SECTION.                                                            
011000     ENTRY 'DLITCBL' USING WDL8-PCB.                                      
011100                                                                          
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011500     PERFORM IMS-GET-WDL8                                                 
011600     PERFORM UNTIL SEGMENT-SAKNAS                                         
011700                                                                          
011800       EVALUATE WDL8-SEG-NAME-FB                                          
011900                                                                          
012000         WHEN 'WDL801'                                                    
012100           IF UT-IDARTNR > 0                                              
012200             PERFORM S11-SKRIV-W21706                                     
012300           END-IF                                                         
012400           MOVE 0 TO UT-KVOI-12-RULL                                      
012500                     UT-KVOI-YEAR-0                                       
012600                     UT-KVOI-YEAR-1                                       
012700                     UT-KVOI-YEAR-2                                       
012800                     UT-KVOI-YEAR-3                                       
012900                     UT-KVOI-YEAR-4                                       
013000                     UT-KVOI-YEAR-5                                       
013100           MOVE ART-IDARTNR TO UT-IDARTNR                                 
013200                                                                          
013300         WHEN 'WDL811'                                                    
013500*          * I SISTA DECEMBERKÖRNINGEN KOMMER KVOI-YEAR-0 ATT VARA        
013600*              NOLL EFTERSOM FILEN ANVÄNDS I JANUARI OCH FÖRE-            
013700*              GÅENDE ÅR SKALL DÅ VARA DET SOM KÖRNINGEN AVSER NU.        
014421           IF AAR-TIAAAA = OI-ARTAL-0                                     
014430*            --- DENNA OVANST.SELEKTION GER                               
014432*            ---                         VERKLIGT INNEVARANDE ÅR          
014440             MOVE 1 TO IX                                                 
014500             PERFORM UNTIL IX > 53                                        
014600               COMPUTE UT-KVOI-YEAR-0 = UT-KVOI-YEAR-0 +                  
014700                       AAR-KVOI-DIV(IX) + AAR-KVOI-NDC(IX) +              
014800                       AAR-KVOI-PROG(IX) +                                
014900                       AAR-KVOI-SATS(IX) + AAR-KVOI-SDC(IX)               
014901                                                                          
014910*              --- SUMMERA NU DE VECKOR SOM TILLHÖR RULLANDE 12.          
014920*              --- KLARAR BÅDE 'FROM' OCH 'TO' UNDER DETTA ÅR.            
014921*              --- OM 'FROM' TILLHÖR FÖREGÅENDE ÅR, FÅNGAS DETTA          
014922*              ---                 I NEDAN SLINGA FÖR OI-ARTAL-1          
014926               IF WS-AA-RULL-TO = AAR-TIAAAA(3:2)                         
014927                 IF IX <= WS-VV-RULL-TO                                   
014928                   COMPUTE UT-KVOI-12-RULL = UT-KVOI-12-RULL +            
014929                         AAR-KVOI-DIV(IX) + AAR-KVOI-NDC(IX) +            
014930                         AAR-KVOI-PROG(IX) +                              
014931                         AAR-KVOI-SATS(IX) + AAR-KVOI-SDC(IX)             
014938                 END-IF                                                   
014940               END-IF                                                     
014950                                                                          
015000               ADD 1 TO IX                                                
015100             END-PERFORM                                                  
015200           END-IF                                                         
015210                                                                          
015300*          --- KVOI 5 ÅR BAKÅT                                            
015400           IF AAR-TIAAAA = OI-ARTAL-5                                     
015500             MOVE 1 TO IX                                                 
015600             PERFORM UNTIL IX > 53                                        
015700               COMPUTE UT-KVOI-YEAR-5  = UT-KVOI-YEAR-5 +                 
015800                       AAR-KVOI-DIV(IX) + AAR-KVOI-NDC(IX) +              
015900                       AAR-KVOI-PROG(IX) +                                
016000                       AAR-KVOI-SATS(IX) + AAR-KVOI-SDC(IX)               
016100               ADD 1 TO IX                                                
016200             END-PERFORM                                                  
016300           END-IF                                                         
016310                                                                          
016320*          --- KVOI 4 ÅR BAKÅT                                            
016500           IF AAR-TIAAAA = OI-ARTAL-4                                     
016600             MOVE 1 TO IX                                                 
016700             PERFORM UNTIL IX > 53                                        
016800               COMPUTE UT-KVOI-YEAR-4  = UT-KVOI-YEAR-4 +                 
016900                       AAR-KVOI-DIV(IX) + AAR-KVOI-NDC(IX) +              
017000                       AAR-KVOI-PROG(IX) +                                
017100                       AAR-KVOI-SATS(IX) + AAR-KVOI-SDC(IX)               
017200               ADD 1 TO IX                                                
017300             END-PERFORM                                                  
017400           END-IF                                                         
017410                                                                          
017420*          --- KVOI 3 ÅR BAKÅT                                            
017600           IF AAR-TIAAAA = OI-ARTAL-3                                     
017700             MOVE 1 TO IX                                                 
017800             PERFORM UNTIL IX > 53                                        
017900               COMPUTE UT-KVOI-YEAR-3  = UT-KVOI-YEAR-3 +                 
018000                       AAR-KVOI-DIV(IX) + AAR-KVOI-NDC(IX) +              
018100                       AAR-KVOI-PROG(IX) +                                
018200                       AAR-KVOI-SATS(IX) + AAR-KVOI-SDC(IX)               
018300               ADD 1 TO IX                                                
018400             END-PERFORM                                                  
018500           END-IF                                                         
018510                                                                          
018520*          --- KVOI 2 ÅR BAKÅT                                            
018700           IF AAR-TIAAAA = OI-ARTAL-2                                     
018800             MOVE 1 TO IX                                                 
018900             PERFORM UNTIL IX > 53                                        
019000               COMPUTE UT-KVOI-YEAR-2  = UT-KVOI-YEAR-2 +                 
019100                       AAR-KVOI-DIV(IX) + AAR-KVOI-NDC(IX) +              
019200                       AAR-KVOI-PROG(IX) +                                
019300                       AAR-KVOI-SATS(IX) + AAR-KVOI-SDC(IX)               
019400               ADD 1 TO IX                                                
019500             END-PERFORM                                                  
019600           END-IF                                                         
019610                                                                          
019620*          --- KVOI FÖRRA ÅRET                                            
019800           IF AAR-TIAAAA = OI-ARTAL-1                                     
019900             MOVE 1 TO IX                                                 
020000             PERFORM UNTIL IX > 53                                        
020100               COMPUTE UT-KVOI-YEAR-1  = UT-KVOI-YEAR-1 +                 
020200                       AAR-KVOI-DIV(IX) + AAR-KVOI-NDC(IX) +              
020300                       AAR-KVOI-PROG(IX) +                                
020400                       AAR-KVOI-SATS(IX) + AAR-KVOI-SDC(IX)               
020410                                                                          
020420*              --- SUMMERA VECKOR SOM TILLHÖR RULLANDE                    
020430               IF WS-AA-RULL-FROM = AAR-TIAAAA(3:2)                       
020440                 IF IX >= WS-VV-RULL-FROM                                 
020450                   COMPUTE UT-KVOI-12-RULL = UT-KVOI-12-RULL +            
020460                         AAR-KVOI-DIV(IX) + AAR-KVOI-NDC(IX) +            
020470                         AAR-KVOI-PROG(IX) +                              
020480                         AAR-KVOI-SATS(IX) + AAR-KVOI-SDC(IX)             
020496                 END-IF                                                   
020497               END-IF                                                     
021400               ADD 1 TO IX                                                
021500             END-PERFORM                                                  
021600           END-IF                                                         
022900       END-EVALUATE                                                       
023000       PERFORM IMS-GET-WDL8                                               
023100     END-PERFORM                                                          
023200     PERFORM Z-FINIT                                                      
023300                                                                          
023400     MOVE ZERO TO RETURN-CODE                                             
023500     GOBACK                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 A-INIT SECTION.                                                          
023900                                                                          
024000     OPEN OUTPUT W21706                                                   
024100                                                                          
024200     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
024202     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024203                                                                          
024204     COMPUTE OI-ARTAL-0 = DAGENS-DATUM-AAR - 0                            
024205     COMPUTE OI-ARTAL-1 = DAGENS-DATUM-AAR - 1                            
024206     COMPUTE OI-ARTAL-2 = DAGENS-DATUM-AAR - 2                            
024207     COMPUTE OI-ARTAL-3 = DAGENS-DATUM-AAR - 3                            
024208     COMPUTE OI-ARTAL-4 = DAGENS-DATUM-AAR - 4                            
024209     COMPUTE OI-ARTAL-5 = DAGENS-DATUM-AAR - 5                            
024210     MOVE ZERO TO UT-IDARTNR                                              
024211                  UT-KVOI-12-RULL                                         
024220                  UT-KVOI-YEAR-0                                          
024230                  UT-KVOI-YEAR-1                                          
024240                  UT-KVOI-YEAR-2                                          
024250                  UT-KVOI-YEAR-3                                          
024260                  UT-KVOI-YEAR-4                                          
024270                  UT-KVOI-YEAR-5                                          
024280                                                                          
024810     MOVE DAGENS-DATUM(3:6) TO  DAT-I-TIDATUM                             
024820     MOVE 'AAMMDD'          TO  DAT-KDDATFORM                             
024840     CALL WDATKONV       USING  DAT-KDDATFORM DAT-I-TIDATUM               
024860                                DAT-O-TIDATUM DAT-KDSVAR                  
024880     IF DAT-KDSVAR-OK                                                     
024881       DISPLAY ' '                                                        
024882       DISPLAY 'DENNA PERIODKÖRNING -----------------------------'        
024883       DISPLAY 'INNEVARANDE RP =' DAT-TIAARP                              
024884       DISPLAY '0I-ÅRTAL-0 ='     OI-ARTAL-0                              
024885       DISPLAY 'DATUM ='          DAGENS-DATUM-DAG                        
024886                                 '/' DAGENS-DATUM-MAANAD                  
024887*      --- STOPPVECKA FÖR OI-RULL-12 ÄR DEN SISTA I DENNA PERIOD          
024888       MOVE DAT-TIAAVVD(1:4)  TO WS-AAVV-RULL-TO                          
024889                                                                          
024890*      --- STARTVECKA FÖR OI-RULL-12 ÄR DEN FÖRSTA 12 PER BAKÅT           
024891       IF DAT-TIAARP (3:2) = 12                                           
024892*        ---                     STARTVECKAN UNDER SAMMA ÅR               
024893         SET  SISTA-PERIODEN  TO TRUE                                     
024894                                                                          
024895         MOVE WS-AAVV-RULL-TO TO WS-AAVV-RULL-FROM                        
024896         MOVE  01             TO WS-AAVV-RULL-FROM(3:2)                   
024899       ELSE                                                               
024900*        ---                     STARTVECKAN ÄR I FÖRRA ÅRET              
024901         SET  VANLIG-PERIOD   TO TRUE                                     
024902                                                                          
024903         COMPUTE DAT-I-TIDATUM = DAT-TIAARP - 99                          
024904*        --- VID FRÅGA PÅ AARP FÅS STARTVECKA I DEN PERIODEN              
024905         MOVE 'AARP  '        TO DAT-KDDATFORM                            
024906         CALL WDATKONV     USING DAT-KDDATFORM DAT-I-TIDATUM              
024907                                 DAT-O-TIDATUM DAT-KDSVAR                 
024908         IF DAT-KDSVAR-OK                                                 
024909           MOVE DAT-TIAAVVD(1:4) TO WS-AAVV-RULL-FROM                     
024910                                                                          
024911         ELSE                                                             
024912           STRING ' FEL FRÅN DATUMRUTIN WDATKONV/ANROP2' STATUS-WS        
024913           DELIMITED BY SIZE INTO FELTEXT                                 
024914           CALL FELLOG                                                    
024915         END-IF                                                           
024916       END-IF                                                             
024917                                                                          
024918       DISPLAY 'RULLANDE 12 STARTVECKA = ' WS-AAVV-RULL-FROM              
024919       DISPLAY '            STOPPVECKA = ' WS-AAVV-RULL-TO                
024920       IF SISTA-PERIODEN                                                  
024921         DISPLAY 'ÅRETS SISTA PERIODKÖRNING (SISTA DECEMBER)'             
024922         DISPLAY 'ADDERAR NU +1 TILL ÅRTALSSIFFRORNA FÖR ATT'             
024923         DISPLAY 'OI-ARTAL-1 SKALL VARA FÖRRA ÅRETS VÄRDEN I JAN.'        
024924         COMPUTE OI-ARTAL-0 = OI-ARTAL-0 + 1                              
024925         COMPUTE OI-ARTAL-1 = OI-ARTAL-1 + 1                              
024926         COMPUTE OI-ARTAL-2 = OI-ARTAL-2 + 1                              
024927         COMPUTE OI-ARTAL-3 = OI-ARTAL-3 + 1                              
024928         COMPUTE OI-ARTAL-4 = OI-ARTAL-4 + 1                              
024929         COMPUTE OI-ARTAL-5 = OI-ARTAL-5 + 1                              
024930       END-IF                                                             
024931                                                                          
024932     ELSE                                                                 
024933       STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                  
024934       DELIMITED BY SIZE INTO FELTEXT                                     
024935       CALL FELLOG                                                        
024940     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 Z-FINIT SECTION.                                                         
030000                                                                          
030100     PERFORM S11-SKRIV-W21706                                             
030200     CLOSE W21706                                                         
030300     SKIP2                                                                
030400     MOVE 'S' TO POSTSUM-OPKOD                                            
030500     CALL POSTSUM USING POSTSUM-PARM                                      
030600     .                                                                    
030700     EJECT                                                                
030800 S11-SKRIV-W21706 SECTION.                                                
030900                                                                          
031000     WRITE UT-POST FROM UT-AREA                                           
031100                                                                          
031200     MOVE '811 '    TO POSTSUM-TRANSTYP                                   
031300     MOVE 'W21706' TO POSTSUM-FDNAMN                                      
031400     MOVE 'W21706D1' TO POSTSUM-DDNAMN2                                   
031500     CALL POSTSUM USING POSTSUM-PARM                                      
031600     .                                                                    
031700     EJECT                                                                
031800* --- IMS SEKTIONER ---                                                   
031900                                                                          
032000                                                                          
032100 IMS-GET-WDL8   SECTION.                                                  
032200                                                                          
032300     CALL CBLTDLI USING GN WDL8-PCB DLI-IO-WDL8                           
032400     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
032500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
032600     PERFORM IMS-STATUSKONTROLL                                           
032700     .                                                                    
032800     EJECT                                                                
032900 IMS-STATUSKONTROLL SECTION.                                              
033000                                                                          
033100     SET STATUS-IX TO 1                                                   
033200     SEARCH GODK-STATUS                                                   
033300       AT END                                                             
033400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033500           DELIMITED BY SIZE INTO FELTEXT                                 
033600         DISPLAY FELTEXT                                                  
033700         CALL FELLOG                                                      
033800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033900         CONTINUE                                                         
034000     END-SEARCH                                                           
034100     .                                                                    
