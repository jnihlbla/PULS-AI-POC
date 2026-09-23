000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2218700.                                                
000400*AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500*DATE-WRITTEN.   93/10/13.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        VECKA OCH PERIOD - LEVERANSPLANER VIA EDI                        
001100*        FRAMSTÄLLER EN FIL MED ARTNR/LEVNR MED ARTIKLAR MED              
001200*        KDAVROP = 2 (GÄLLANDE AVROP).OBS! BARA CDC.                      
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- ARTIKLAR MED KDAVROP = 2                                   
002900     SELECT W22187                     ASSIGN TO W22187D1.                
003000     SKIP3                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W22187                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900*01  POST -COPY W22187 -PRE  UT-  -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W2218700'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004801 77  SKRIV-W22187-SW             PIC X       VALUE 'N'.                   
004802     88 SKRIV-W22187                         VALUE 'J'.                   
004803                                                                          
004810     SKIP3                                                                
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005400     EJECT                                                                
005500*      --- VALID IDDC CODES                                               
005600*01  -COPY WWDCKONS                                                       
005700     EJECT                                                                
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900*                                                                         
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     SKIP2                                                                
006500*    --- PARAMETRAR TILL ABEND                                            
006600                                                                          
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL POSTSUM                                          
007500*                                                                         
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800 01  UT-AREA-START               PIC X(24)   VALUE                        
007900                                 'UT-AREA-START  '.                       
008000     SKIP2                                                                
008100                                                                          
008200*01  AREA -COPY W22187     -PRE UT-                                       
008300     EJECT                                                                
008400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  NYCKLAR-TILL-DLI.                                                    
008900     03  W-IDARTNR-X.                                                     
009000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009100     03  W-IDLEVNR-X.                                                     
009200         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
009300     SKIP2                                                                
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
009800     SKIP2                                                                
009900 01  GODK-STATUSKODER.                                                    
010000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010100     SKIP3                                                                
010200 01  SSA1                        PIC X(64).                               
010300 01  SSA2                        PIC X(64).                               
010400     EJECT                                                                
010500*    --- IMS FUNKTIONSKODER                                               
010600*01  -COPY W0003                                                          
010700     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
010900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011000     SKIP3                                                                
011100 01  DLI-IO-AREA.                                                         
011200     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011300     SKIP3                                                                
011400     03  WLINLB01 REDEFINES IO-AREA.                                      
011500*        05  -COPY WDD901  -PRE INLB-                                     
011600     SKIP3                                                                
011700     03  WLINLB11 REDEFINES IO-AREA.                                      
011800*        05  -COPY WDD902  -PRE INLB-                                     
011810     SKIP3                                                                
011820     03  WLINLB23 REDEFINES IO-AREA.                                      
011830*        05  -COPY WDD905  -PRE INLB-                                     
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100                                                                          
012200     SKIP2                                                                
012300*01  -COPY W0008  -PRE INLB-                                              
012400     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION  USING INLB-PCB.                                      
012700     ENTRY 'DLITCBL' USING INLB-PCB.                                      
012800                                                                          
012900     SKIP2                                                                
013000     PERFORM A-INIT                                                       
013100     PERFORM IMS-GET-INLB                                                 
013200                                                                          
013300     PERFORM UNTIL SEGMENT-SAKNAS                                         
013400        IF INLB-SEG-NAME-FB = 'WDD901  '                                  
013500           IF INLB-IDDC = WC-CDC-SE                                       
013510             MOVE JA           TO SKRIV-W22187-SW                         
013600             MOVE INLB-IDARTNR TO W-IDARTNR                               
013610           ELSE                                                           
013620             MOVE NEJ          TO SKRIV-W22187-SW                         
013700           END-IF                                                         
013710        END-IF                                                            
013800                                                                          
013900        IF INLB-SEG-NAME-FB = 'WDD902  '                                  
013910           IF SKRIV-W22187                                                
014000             MOVE INLB-IDLEVNR TO W-IDLEVNR                               
014100           END-IF                                                         
014400        END-IF                                                            
014401                                                                          
014410        IF INLB-SEG-NAME-FB = 'WDD905  '                                  
014411           IF SKRIV-W22187                                                
014430             IF INLB-KDAVROP = 2                                          
014431               IF W-IDARTNR = UT-IDARTNR AND                              
014433                  W-IDLEVNR = UT-IDLEVNR                                  
014434                                                                          
014435                  CONTINUE                                                
014436               ELSE                                                       
014437                 MOVE W-IDARTNR TO UT-IDARTNR                             
014438                 MOVE W-IDLEVNR TO UT-IDLEVNR                             
014439                                                                          
014440                 PERFORM S11-SKRIV-W22187                                 
014450               END-IF                                                     
014451             END-IF                                                       
014452           END-IF                                                         
014460        END-IF                                                            
014500                                                                          
014600        PERFORM IMS-GET-INLB                                              
014700     END-PERFORM                                                          
014800                                                                          
014900                                                                          
015000     PERFORM Z-FINIT                                                      
015100                                                                          
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015700                                                                          
015800     OPEN OUTPUT W22187                                                   
015900     SKIP2                                                                
016000     ACCEPT DAGENS-DATUM  FROM DATE                                       
016100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016110                                                                          
016120     MOVE ZERO   TO UT-IDARTNR                                            
016130     MOVE SPACE  TO UT-IDLEVNR                                            
016200     .                                                                    
016300     EJECT                                                                
016400 Z-FINIT SECTION.                                                         
016500     CLOSE W22187                                                         
016600     SKIP2                                                                
016700     MOVE 'S' TO POSTSUM-OPKOD                                            
016800     CALL POSTSUM USING POSTSUM-PARM                                      
016900     .                                                                    
017000     EJECT                                                                
017100 S11-SKRIV-W22187 SECTION.                                                
017200     SKIP2                                                                
017300*    UTSKRIFT AV SAMTLIGA ARTIKLAR MED KDAVROP = 2                        
017310*    OBS! TOMPLANER SKALL BARA KOMMA PÅ ÄNDRINGSFILEN                     
017400                                                                          
017500     WRITE UT-POST FROM UT-AREA                                           
017600                                                                          
017700     MOVE 'REC'      TO POSTSUM-TRANSTYP                                  
017800     MOVE 'W22187'   TO POSTSUM-FDNAMN                                    
017900     MOVE 'W22187D1' TO POSTSUM-DDNAMN2                                   
018000     CALL POSTSUM USING POSTSUM-PARM                                      
018100     .                                                                    
018200     EJECT                                                                
018300 S99-ABEND SECTION.                                                       
018400     SKIP2                                                                
018500     SKIP2                                                                
018600     MOVE 'S' TO POSTSUM-OPKOD                                            
018700     CALL POSTSUM USING POSTSUM-PARM                                      
018800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
018900     .                                                                    
019000     EJECT                                                                
019100* --- IMS SEKTIONER ---                                                   
019200     SKIP3                                                                
019300 IMS-GET-INLB   SECTION.                                                  
019400     SKIP2                                                                
019500     CALL CBLTDLI USING GN INLB-PCB DLI-IO-AREA                           
019600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
019700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
019800     PERFORM IMS-STATUSKONTROLL                                           
019900     .                                                                    
020000     EJECT                                                                
020100 IMS-STATUSKONTROLL SECTION.                                              
020200     SKIP2                                                                
020300     SET STATUS-IX TO 1                                                   
020400     SEARCH GODK-STATUS                                                   
020500       AT END                                                             
020600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
020700         DISPLAY FELTEXT                                                  
020800         CALL FELLOG                                                      
020900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021000         CONTINUE                                                         
021100     END-SEARCH                                                           
021200     .                                                                    
