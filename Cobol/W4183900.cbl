000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4183900.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   08/04/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER WDR4/IDHTYP=4101/4102 MED SB OCH                
000900*        SKAPAR 2 ST UTFILER. W418AU MED RADER SOM SKALL UPPDATERA        
001000*        WDGX4102 MED JA I FLAGGA FLFAKT. W418AV MED DE RAPPORTER         
001100*        SOM SKALL FAKTURERAS FÖR HANTERINGSKOSTNAD MED 100 SEK           
001200*        PER RETURNERAD RAD.STORLEKEN PÅ AVGIFTEN STYRS FRÅN EN NY        
001300*        PARAMETERBILD AV VCCS.                                           
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDR4                                       
001600*                                                                         
001610*    E'TRACKER 880053 DATED 2008-04-27                                    
001620*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- UPPDATERINGSPOSTER WDGX4102                                
003000     SELECT W418AU                     ASSIGN TO W41839D1.                
003100     SKIP2                                                                
003200*          --- FAKTURARADER TILL BILL-IT, HANDLING FEE                    
003300     SELECT W418AV                     ASSIGN TO W41839D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W418AU                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY W418AU -PRE  UTAU-  -L.                                   
004400     SKIP3                                                                
004500 FD  W418AV                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  POST -COPY W418FEE -PRE  UTAV-  -L.                                  
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W4183900'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  WS-IDHTYP                   PIC X(4)    VALUE SPACE.                 
005700 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005800 77  WS-IDDISTR                  PIC S9(5)   COMP-3 VALUE +0.             
005900 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
006000 77  SPAR-IDDISTR                PIC S9(5)   COMP-3 VALUE +0.             
006100 77  SPAR-IDKUNDNR               PIC S9(7)   COMP-3 VALUE +0.             
006200 77  SPAR-IDRAPP                 PIC X(10)   VALUE SPACE.                 
006300 77  FOERSTA-IDRAPP-SW           PIC X       VALUE 'J'.                   
006400 77  POSTER-FINNS-SW             PIC X       VALUE 'N'.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     SKIP2                                                                
007900*    --- PARAMETRAR TILL ABEND                                            
008000                                                                          
008100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008400     SKIP2                                                                
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL POSTSUM                                          
009000*                                                                         
009100*01  -COPY W0005   -PRE  POSTSUM-                                         
009200     EJECT                                                                
009300 01  UTAU-AREA-START             PIC X(24)   VALUE                        
009400                                 'UTAU-AREA-START  '.                     
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY W418AU     -PRE UTAU-                                     
009800     EJECT                                                                
009900 01  UTAV-AREA-START             PIC X(24)   VALUE                        
010000                                 'UTAV-AREA-START  '.                     
010100     SKIP2                                                                
010200                                                                          
010300*01  AREA -COPY W418FEE     -PRE UTAV-                                    
010400     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-WDGXKEY-X.                                                     
011200         05  W-WDGXKEY           PIC X(30)    VALUE SPACE.                
011300     03  W-KY4102-X.                                                      
011400         05  W-KY4102            PIC X(22)    VALUE SPACE.                
011500     SKIP2                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012000     SKIP2                                                                
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300     SKIP3                                                                
012400 01  SSA1                        PIC X(64).                               
012500 01  SSA2                        PIC X(64).                               
012600     EJECT                                                                
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
013200 01  DLI-IO-AREA.                                                         
013300     03  IO-AREA                 PIC X(200).                              
013400*    03  -COPY WDGX01   -RED IO-AREA.                                     
013500*    03  -COPY WDGX4101 -RED IO-AREA.                                     
013600*    03  -COPY WDGX4102 -RED IO-AREA.                                     
013700     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900                                                                          
014000*01  -COPY W0008  -PRE WDR4-                                              
014100     05  FILLER                  PIC X.                                   
014200     EJECT                                                                
014300 PROCEDURE DIVISION  USING WDR4-PCB.                                      
014400 MAIN SECTION.                                                            
014500     ENTRY 'DLITCBL' USING WDR4-PCB.                                      
014600                                                                          
014700     PERFORM A-INIT                                                       
014800                                                                          
014900     PERFORM IMS-GET-WDR4                                                 
015000     PERFORM UNTIL SEGMENT-SLUT                                           
015100       EVALUATE WDR4-SEG-NAME-FB                                          
015200         WHEN 'WDR401'                                                    
015300           MOVE IDHTYP              TO WS-IDHTYP                          
015400           IF WS-IDHTYP = '4101'                                          
015500             MOVE 4101-IDDC         TO WS-IDDC                            
015600             MOVE 4101-IDDISTR      TO WS-IDDISTR                         
015700           END-IF                                                         
015800         WHEN 'WDGX4102'                                                  
015900           IF 4102-FLFAKT = 'N' AND 4102-DARFSDAT > ZERO                  
016000             MOVE JA TO POSTER-FINNS-SW                                   
016100             PERFORM B-SKAPA-UPPDAT-RADER-4102                            
016200             PERFORM C-SKAPA-FAKTURA-RADER                                
016300           END-IF                                                         
016400       END-EVALUATE                                                       
016500       PERFORM IMS-GET-WDR4                                               
016600     END-PERFORM                                                          
016700                                                                          
016800     IF POSTER-FINNS-SW = JA                                              
016900       PERFORM S12-SKRIV-W418AV                                           
017000     END-IF                                                               
017100                                                                          
017200     PERFORM Z-FINIT                                                      
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 A-INIT SECTION.                                                          
017900                                                                          
018000     OPEN OUTPUT W418AU                                                   
018100                 W418AV                                                   
018200                                                                          
018300     MOVE 0              TO UTAV-RP-KVRADER-RET                           
018400     MOVE JA             TO FOERSTA-IDRAPP-SW                             
018500     MOVE NEJ            TO POSTER-FINNS-SW                               
018600                                                                          
018700     ACCEPT DAGENS-DATUM  FROM DATE                                       
018800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018900     .                                                                    
019000     EJECT                                                                
019100 B-SKAPA-UPPDAT-RADER-4102  SECTION.                                      
019200                                                                          
019300     MOVE WS-IDHTYP         TO UTAU-IDHTYP                                
019400     MOVE WS-IDDC           TO UTAU-IDDC                                  
019500     MOVE WS-IDDISTR        TO UTAU-IDDISTR                               
019600     MOVE 4102-IDKUNDNR     TO UTAU-IDKUNDNR                              
019700     MOVE 4102-IDRAPP       TO UTAU-IDRAPP                                
019800     MOVE 4102-IDKOLLI      TO UTAU-IDKOLLI                               
019900     MOVE 4102-IDARTNR      TO UTAU-IDARTNR                               
020000     MOVE 4102-DAREGDAT     TO UTAU-DAREGDAT                              
020100     MOVE 4102-DARFSDAT     TO UTAU-DARFSDAT                              
020200     MOVE 4102-IDAVS        TO UTAU-IDAVS                                 
020300     MOVE 4102-KDFEL        TO UTAU-KDFEL                                 
020400     MOVE 4102-KVANTAL      TO UTAU-KVANTAL                               
020500     MOVE 4102-TENOTE       TO UTAU-TENOTE                                
020600                                                                          
020700     PERFORM S11-SKRIV-W418AU                                             
020800     .                                                                    
020900     EJECT                                                                
021000 C-SKAPA-FAKTURA-RADER  SECTION.                                          
021100                                                                          
021200     IF WS-IDDC       = SPAR-IDDC     AND                                 
021300        WS-IDDISTR    = SPAR-IDDISTR  AND                                 
021400        4102-IDKUNDNR = SPAR-IDKUNDNR AND                                 
021500        4102-IDRAPP   = SPAR-IDRAPP                                       
021600                                                                          
021700        ADD +1                TO UTAV-RP-KVRADER-RET                      
021800     ELSE                                                                 
021900       IF FOERSTA-IDRAPP-SW = JA                                          
022000         MOVE NEJ             TO FOERSTA-IDRAPP-SW                        
022100       ELSE                                                               
022200         PERFORM S12-SKRIV-W418AV                                         
022300       END-IF                                                             
022400                                                                          
022500       MOVE WS-IDDC           TO  SPAR-IDDC                               
022600       MOVE WS-IDDISTR        TO  SPAR-IDDISTR                            
022700       MOVE 4102-IDKUNDNR     TO  SPAR-IDKUNDNR                           
022800       MOVE 4102-IDRAPP       TO  SPAR-IDRAPP                             
022900                                                                          
023000                                                                          
023100       MOVE 'ROR'             TO UTAV-RP-IDPTYP                           
023200       MOVE WS-IDDC           TO UTAV-RP-IDDC                             
023300       MOVE WS-IDDISTR        TO UTAV-RP-IDDISTR                          
023400                                                                          
023500       MOVE 4102-IDKUNDNR     TO UTAV-RP-IDKUNDNR                         
023600       MOVE ZERO              TO UTAV-RP-IDRAPPNR                         
023700       MOVE 4102-IDRAPP       TO UTAV-RP-IDRAPP                           
023800       MOVE ZERO              TO UTAV-RP-KVRADER-72                       
023900       MOVE ZERO              TO UTAV-RP-KVRADER-98                       
024000       MOVE ZERO              TO UTAV-RP-DARETANK                         
024200       MOVE 4102-DARFSDAT     TO UTAV-RP-DARFSDAT                         
024300                                                                          
024400       MOVE 1                 TO UTAV-RP-KVRADER-RET                      
024500                                                                          
024600     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 Z-FINIT SECTION.                                                         
025000     CLOSE W418AU                                                         
025100           W418AV                                                         
025200     SKIP2                                                                
025300     MOVE 'S' TO POSTSUM-OPKOD                                            
025400     CALL POSTSUM USING POSTSUM-PARM                                      
025500     .                                                                    
025600     EJECT                                                                
025700 S11-SKRIV-W418AU SECTION.                                                
025800                                                                          
025900     WRITE UTAU-POST FROM UTAU-AREA                                       
026000                                                                          
026100     MOVE UTAU-IDHTYP TO POSTSUM-TRANSTYP                                 
026200     MOVE 'W418AU' TO POSTSUM-FDNAMN                                      
026300     MOVE 'W41839D1' TO POSTSUM-DDNAMN2                                   
026400     CALL POSTSUM USING POSTSUM-PARM                                      
026500     .                                                                    
026600     EJECT                                                                
026700 S12-SKRIV-W418AV SECTION.                                                
026800                                                                          
026900     WRITE UTAV-POST FROM UTAV-AREA                                       
027000                                                                          
027100     MOVE UTAV-RP-IDPTYP TO POSTSUM-TRANSTYP                              
027200     MOVE 'W418AV' TO POSTSUM-FDNAMN                                      
027300     MOVE 'W41839D2' TO POSTSUM-DDNAMN2                                   
027400     CALL POSTSUM USING POSTSUM-PARM                                      
027500     .                                                                    
027600     EJECT                                                                
027700 S99-ABEND SECTION.                                                       
027800                                                                          
027900     SKIP2                                                                
028000     MOVE 'S' TO POSTSUM-OPKOD                                            
028100     CALL POSTSUM USING POSTSUM-PARM                                      
028200     CALL ABEND USING RKOD-ABEND                                          
028300     .                                                                    
028400     EJECT                                                                
028500* --- IMS SEKTIONER ---                                                   
028600                                                                          
028700                                                                          
028800 IMS-GET-WDR4   SECTION.                                                  
028900                                                                          
029000     CALL CBLTDLI USING GN WDR4-PCB DLI-IO-AREA                           
029100     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
029200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
029300     PERFORM IMS-STATUSKONTROLL                                           
029400     .                                                                    
029500     EJECT                                                                
029600 IMS-STATUSKONTROLL SECTION.                                              
029700                                                                          
029800     SET STATUS-IX TO 1                                                   
029900     SEARCH GODK-STATUS                                                   
030000       AT END                                                             
030100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030200           DELIMITED BY SIZE INTO FELTEXT                                 
030300         DISPLAY FELTEXT                                                  
030400         CALL FELLOG                                                      
030500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030600         CONTINUE                                                         
030700     END-SEARCH                                                           
030800     .                                                                    
