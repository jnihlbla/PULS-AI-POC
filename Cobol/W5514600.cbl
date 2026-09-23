001000 ID DIVISION.                                                             
001100                                                                          
001200 PROGRAM-ID.     W5514600.                                                
001300 AUTHOR.         THOMAS LARSSON.                                          
001400 DATE-WRITTEN.   94/05/04.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        PROGRAMMET LÄSER NER SATSBASEN WDJ1.UTFILEN SORTERAS PÅ          
001900*        INGÅENDE ARTIKELNR.                                              
002000*                                                                         
002110*        PROGRAMMET LÄSER      WLSATB (WDJ1)                              
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- FIL MED SATSINFO FRÅN WDJ1                                 
003410     SELECT W55146                     ASSIGN TO W55146D1.                
003500     SKIP2                                                                
003600*          --- SORTERINGSFIL                                              
003700     SELECT SORTFIL                    ASSIGN TO W55146DS.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004301     SKIP3                                                                
004302 FD  W55146                                                               
004303     RECORDING       F                                                    
004304     BLOCK CONTAINS  0.                                                   
004305     SKIP2                                                                
004310*01  POST -COPY W55146 -PRE  UT-  -L.                                     
004400     SKIP3                                                                
004500 SD  SORTFIL                                                              
004600     RECORDING       F                                                    
004610     SKIP2                                                                
004620*01  POST -COPY W55146 -PRE  SORT-                                        
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
004901*    -COPY WY2000W1                                                       
004910     SKIP3                                                                
005000 77  IDPGM                       PIC X(8)    VALUE 'W5514600'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
005410     88  SKRIV-POST                          VALUE 'J'.                   
005420                                                                          
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006710     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  FELTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007701     EJECT                                                                
007702*    --- PARAMETRAR TILL POSTSUM                                          
007703*                                                                         
007710*01  -COPY W0005   -PRE  POSTSUM-                                         
007901     EJECT                                                                
007902 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
007903                                 'SORTWS-AREA-START  '.                   
007904     SKIP2                                                                
007905                                                                          
007910*01  AREA -COPY W55146     -PRE SORTWS-                                   
007920     SKIP2                                                                
008100 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
008200     EJECT                                                                
008300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*                                                                         
008500     SKIP2                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  NYCKLAR-TILL-DLI.                                                    
008901     03  W-IDARTNR-X.                                                     
008902         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009000     SKIP2                                                                
009100*    --- STATUS-KOD FRÅN IMS                                              
009200 01  STATUS-WS                   PIC XX.                                  
009300     88  SEGMENT-FINNS                       VALUE '  '.                  
009400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009500     SKIP2                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010800     SKIP3                                                                
010900 01  DLI-IO-AREA.                                                         
011000     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
011101     SKIP3                                                                
011102     03  WLSATB01 REDEFINES IO-AREA.                                      
011103*        05  -COPY WDJ101  -PRE SATB01-                                   
011104     SKIP3                                                                
011105     03  WLSATB11 REDEFINES IO-AREA.                                      
011110*        05  -COPY WDJ111  -PRE SATB11-                                   
011400     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011600                                                                          
011701     SKIP2                                                                
011702*01  -COPY W0008  -PRE SATB-                                              
011710     05  FILLER                  PIC X.                                   
011800     EJECT                                                                
011901 PROCEDURE DIVISION  USING SATB-PCB.                                      
011910     ENTRY 'DLITCBL' USING SATB-PCB.                                      
012000                                                                          
012200     SKIP2                                                                
012300     PERFORM A-INIT                                                       
012400                                                                          
012500     SORT SORTFIL ASCENDING KEY SORT-IDARTNR-ING                          
012700                  INPUT PROCEDURE B-SORT-INPUT                            
012810                  GIVING W55146                                           
012900                                                                          
013000     IF SORT-RETURN NOT = 0                                               
013100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
013200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
013300           DELIMITED BY SIZE                                              
013400           INTO FELTEXT-STR                                               
013500       DISPLAY FELTEXT                                                    
013600       PERFORM S99-ABEND                                                  
013700     ELSE                                                                 
013800       PERFORM Z-FINIT                                                    
013900                                                                          
014000       MOVE ZERO TO RETURN-CODE                                           
014100       GOBACK                                                             
014200     END-IF                                                               
014300                                                                          
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014900     SKIP2                                                                
015000     ACCEPT DAGENS-DATUM  FROM DATE                                       
015110     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015111     MOVE ZERO TO SORTWS-IDARTNR-SATS                                     
015112                  SORTWS-IDARTNR-ING                                      
015113                  SORTWS-REANTPSA                                         
015300     .                                                                    
015500     EJECT                                                                
015510 B-SORT-INPUT SECTION.                                                    
015520     SKIP2                                                                
015530     PERFORM IMS-GET-WDJ1                                                 
015540     PERFORM UNTIL SEGMENT-SLUT                                           
015550       EVALUATE SATB-SEG-NAME-FB                                          
015560         WHEN 'WDJ101  '                                                  
015591           IF SATB01-STR-TIBORT = ZERO AND                                
015592              SATB01-STR-IDARTNR < 100000000 AND                          
015593              SATB01-STR-IDLEVNR = '1002'                                 
015594              MOVE SATB01-STR-IDARTNR    TO SORTWS-IDARTNR-SATS           
015595              MOVE JA TO SKRIV-SW                                         
015596           ELSE                                                           
015597              MOVE NEJ TO SKRIV-SW                                        
015598           END-IF                                                         
015599         WHEN 'WDJ111  '                                                  
015601           MOVE SATB11-RAD-TISTADAT   TO TMP1-YYMMDD                      
015602           MOVE SATB11-RAD-TISTODAT   TO TMP2-YYMMDD                      
015603           MOVE DAGENS-DATUM          TO TMP3-YYMMDD                      
015604           PERFORM WY2000Q1                                               
015605           IF (TMP1-YYMMDD <= TMP3-YYMMDD AND                             
015606               TMP2-YYMMDD > TMP3-YYMMDD)                                 
015607               MOVE SATB11-RAD-REANTPSA  TO SORTWS-REANTPSA               
015608               MOVE SATB11-RAD-IDARTNR   TO SORTWS-IDARTNR-ING            
015609               IF SKRIV-POST                                              
015610                 PERFORM S31-RELEASE-W55146                               
015611               END-IF                                                     
015612           END-IF                                                         
015613       END-EVALUATE                                                       
015614       PERFORM IMS-GET-WDJ1                                               
015615     END-PERFORM                                                          
015616     .                                                                    
015620     EJECT                                                                
015700 Z-FINIT SECTION.                                                         
015801     SKIP2                                                                
015802     MOVE 'S' TO POSTSUM-OPKOD                                            
015810     CALL POSTSUM USING POSTSUM-PARM                                      
015900     .                                                                    
016300     EJECT                                                                
016400 S31-RELEASE-W55146  SECTION.                                             
016500     SKIP2                                                                
016600     RELEASE SORT-POST FROM SORTWS-AREA                                   
016601                                                                          
016602     MOVE 'WDJ1'  TO POSTSUM-TRANSTYP                                     
016603     MOVE 'W55146' TO POSTSUM-FDNAMN                                      
016604     MOVE 'W55146D1' TO POSTSUM-DDNAMN2                                   
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016700     .                                                                    
016800     EJECT                                                                
016900 S99-ABEND SECTION.                                                       
017000     SKIP2                                                                
017101     SKIP2                                                                
017102     MOVE 'S' TO POSTSUM-OPKOD                                            
017110     CALL POSTSUM USING POSTSUM-PARM                                      
017200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
017300     .                                                                    
017400     EJECT                                                                
017500* --- IMS SEKTIONER ---                                                   
017600     SKIP3                                                                
017702 IMS-GET-WDJ1   SECTION.                                                  
017703     SKIP2                                                                
017704     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA                           
017705     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
017706     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
017707     PERFORM IMS-STATUSKONTROLL                                           
017710     .                                                                    
017800     EJECT                                                                
017900 IMS-STATUSKONTROLL SECTION.                                              
018000     SKIP2                                                                
018100     SET STATUS-IX TO 1                                                   
018200     SEARCH GODK-STATUS                                                   
018300       AT END                                                             
018400         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
018500         DISPLAY FELTEXT                                                  
018600         CALL FELLOG                                                      
018700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
018800         CONTINUE                                                         
018900     END-SEARCH                                                           
019000     .                                                                    
019010     EJECT                                                                
019100*    -COPY WY2000Q1                                                       
