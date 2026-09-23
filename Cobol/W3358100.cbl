000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3358100.                                                
000300 AUTHOR.         SMITH GAVIN.                                             
000400 DATE-WRITTEN.   99/06/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        EXTRACTS PARTS FROM WDD3 AND CREATES FILE WITH 18                
000900*        LANGUAGES FOR EXPORT TO AS 400 MCPRICE SYSTEMS                   
001000*        PROGRAM RUN 1 TIME / MONTH.                                      
001100*        A SUM OF NO OF TRANS PFILE IS ALSO GENERATED.                    
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*                              WDK6                                       
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- OUTPUTFILE  MULTILANGUAGE TO MCPRICE / MONTH               
002900     SELECT W33581                     ASSIGN TO W33581D1.                
003000*          --- OUTPUTFILE  NO OF RECORDS ON FILE                          
003100     SELECT W33582                     ASSIGN TO W33581D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W33581                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST -COPY W33581 -PRE  UT-  -L.                                     
004200     SKIP3                                                                
004300 FD  W33582                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W33581 -PRE  TOT- -L.                                     
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100                                                                          
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W3358100'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  RAKNARE                     PIC 9(2)    VALUE ZERO.                  
005700 77  RAKNARE-TOT                 PIC 9(9)    VALUE ZERO.                  
005800     EJECT                                                                
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     SKIP2                                                                
006600*    --- PARAMETRAR TILL ABEND                                            
006700                                                                          
006800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007100     SKIP2                                                                
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL POSTSUM                                          
007700*                                                                         
007800*01  -COPY W0005   -PRE  POSTSUM-                                         
007900     EJECT                                                                
008000*                                                                         
008100*01  -COPY WWPRODSL                                                       
008200     EJECT                                                                
008300 01  UT-AREA-START               PIC X(24)   VALUE                        
008400                                 'UT-AREA-START  '.                       
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W33581     -PRE UT-                                       
008800     SKIP2                                                                
008900                                                                          
009000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009100*                                                                         
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009400     SKIP3                                                                
009500*    --- STATUS-KOD FRÅN IMS                                              
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FINNS                       VALUE '  '.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
009900     SKIP2                                                                
010000 01  GODK-STATUSKODER.                                                    
010100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(64).                               
010500 01  NYCKLAR-TILL-DLI.                                                    
010600     03  W-IDARTNR-X.                                                     
010700         05  W-IDARTNR           PIC S9(9)   VALUE +0  COMP-3.            
010800     EJECT                                                                
010900*    --- IMS FUNKTIONSKODER                                               
011000*01  -COPY W0003                                                          
011100     EJECT                                                                
011200*    ---  DLI INPUT-OUTPUT AREA                                           
011300 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
011400 01  DLI-IO-AREA.                                                         
011500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011600     SKIP3                                                                
011700     03  WLBENA01 REDEFINES IO-AREA.                                      
011800*        05  -COPY WDD301  -PRE SEG1-                                     
011900     SKIP3                                                                
012000     03  WLBENA11 REDEFINES IO-AREA.                                      
012100*        05  -COPY WDD311  -PRE SEG11-                                    
012200     SKIP3                                                                
012300     03  WLBENA12 REDEFINES IO-AREA.                                      
012400*        05  -COPY WDD312  -PRE SEG12-                                    
012500     EJECT                                                                
012600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK601'.           
012700 01  DLI-IO-WDK601.                                                       
012800*    03  -COPY WDK601                                                     
012900 LINKAGE SECTION.                                                         
013000                                                                          
013100                                                                          
013200*01  -COPY W0008  -PRE BENA-                                              
013300     05  FILLER                  PIC X.                                   
013400     EJECT                                                                
013500*01  -COPY W0008  -PRE WDK6-                                              
013600     05  FILLER                  PIC X.                                   
013700     EJECT                                                                
013800 PROCEDURE DIVISION  USING BENA-PCB WDK6-PCB.                             
013900 MAIN SECTION.                                                            
014000     ENTRY 'DLITCBL' USING BENA-PCB WDK6-PCB.                             
014100                                                                          
014200                                                                          
014300     PERFORM A-INIT                                                       
014400                                                                          
014500     PERFORM IMS-GET-BENA                                                 
014600     PERFORM UNTIL SEGMENT-SAKNAS                                         
014700       EVALUATE BENA-SEG-NAME-FB                                          
014800         WHEN 'WDD301'                                                    
014900           PERFORM B-NOLLSTAELL-FAELT                                     
015000         WHEN 'WDD311'                                                    
015100           PERFORM C-FYLLA-UTAREA                                         
015200         WHEN 'WDD312'                                                    
015300           PERFORM D-SKRIV-UTFIL                                          
015400       END-EVALUATE                                                       
015500       PERFORM IMS-GET-BENA                                               
015600     END-PERFORM                                                          
015700     PERFORM E-SKRIV-TOTFIL                                               
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500                                                                          
016600     OPEN OUTPUT W33581                                                   
016700                 W33582                                                   
016800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900     MOVE ZERO TO RAKNARE-TOT                                             
017000     .                                                                    
017100     EJECT                                                                
017200 B-NOLLSTAELL-FAELT SECTION.                                              
017300                                                                          
017400     MOVE ZERO TO RAKNARE                                                 
017500     MOVE SPACE  TO UT-AREA                                               
017600     .                                                                    
017700     EJECT                                                                
017800 C-FYLLA-UTAREA SECTION.                                                  
017900     IF SEG11-TEXT-IDSKYLT = 'GB '                                        
018000       MOVE 1 TO RAKNARE                                                  
018100       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
018200     END-IF                                                               
018300     IF SEG11-TEXT-IDSKYLT = 'USA'                                        
018400       MOVE 2 TO RAKNARE                                                  
018500       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
018600     END-IF                                                               
018700     IF SEG11-TEXT-IDSKYLT = 'F  '                                        
018800       MOVE 3 TO RAKNARE                                                  
018900       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
019000     END-IF                                                               
019100     IF SEG11-TEXT-IDSKYLT = 'D  '                                        
019200       MOVE 4 TO RAKNARE                                                  
019300       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
019400     END-IF                                                               
019500     IF SEG11-TEXT-IDSKYLT = 'E  '                                        
019600       MOVE 5 TO RAKNARE                                                  
019700       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
019800     END-IF                                                               
019900     IF SEG11-TEXT-IDSKYLT = 'I  '                                        
020000       MOVE 6 TO RAKNARE                                                  
020100       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
020200     END-IF                                                               
020300     IF SEG11-TEXT-IDSKYLT = 'P  '                                        
020400       MOVE 7 TO RAKNARE                                                  
020500       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
020600     END-IF                                                               
020700     IF SEG11-TEXT-IDSKYLT = 'RUS'                                        
020800       MOVE 8 TO RAKNARE                                                  
020900       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
021000     END-IF                                                               
021100     IF SEG11-TEXT-IDSKYLT = 'NL '                                        
021200       MOVE 9 TO RAKNARE                                                  
021300       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
021400     END-IF                                                               
021500     IF SEG11-TEXT-IDSKYLT = 'S  '                                        
021600       MOVE 10 TO RAKNARE                                                 
021700       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
021800     END-IF                                                               
021900     IF SEG11-TEXT-IDSKYLT = 'SF '                                        
022000       MOVE 11 TO RAKNARE                                                 
022100       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
022200     END-IF                                                               
022300     IF SEG11-TEXT-IDSKYLT = 'J  '                                        
022400       MOVE 12 TO RAKNARE                                                 
022500       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
022600     END-IF                                                               
022700     IF SEG11-TEXT-IDSKYLT = 'RC '                                        
022800       MOVE 13 TO RAKNARE                                                 
022900       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
023000     END-IF                                                               
023100     IF SEG11-TEXT-IDSKYLT = 'KOR'                                        
023200       MOVE 14 TO RAKNARE                                                 
023300       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
023400     END-IF                                                               
023500     IF SEG11-TEXT-IDSKYLT = 'MAL'                                        
023600       MOVE 15 TO RAKNARE                                                 
023700       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
023800     END-IF                                                               
023900     IF SEG11-TEXT-IDSKYLT = 'T  '                                        
024000       MOVE 16 TO RAKNARE                                                 
024100       MOVE SEG11-TEXT-BEART TO UT-BEART (RAKNARE)                        
024200     END-IF                                                               
024300     .                                                                    
024400     EJECT                                                                
024500 D-SKRIV-UTFIL SECTION.                                                   
024600     IF SEG12-ART-IDARTNR > 0                                             
024700       MOVE SEG12-ART-IDARTNR TO UT-IDARTNR                               
024800                                 W-IDARTNR                                
024900       PERFORM IMS-GU-WDK601                                              
025000       IF SEGMENT-FINNS                                                   
025100         MOVE ART-KDPRODSL TO TEST-KDPRODSL                               
025200*        IF KDPRODSL-LYNK                                                 
025300*DO NOT SEND LYNK PART TO UT FILE AND SO THE TOTAL COUNT                  
025400*          CONTINUE                                                       
025500*        ELSE                                                             
025600           PERFORM S11-SKRIV-W33581                                       
025700           ADD 1 TO RAKNARE-TOT                                           
025800*        END-IF                                                           
025900       ELSE                                                               
026000         PERFORM S11-SKRIV-W33581                                         
026100         ADD 1 TO RAKNARE-TOT                                             
026200       END-IF                                                             
026300     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 E-SKRIV-TOTFIL SECTION.                                                  
026700     MOVE SPACE TO UT-AREA                                                
026800     MOVE RAKNARE-TOT       TO UT-IDARTNR                                 
026900     PERFORM S12-SKRIV-W33582                                             
027000     MOVE ZERO  TO RAKNARE-TOT                                            
027100     .                                                                    
027200     EJECT                                                                
027300 Z-FINIT SECTION.                                                         
027400     CLOSE W33581                                                         
027500           W33582                                                         
027600     SKIP2                                                                
027700     MOVE 'S' TO POSTSUM-OPKOD                                            
027800     CALL POSTSUM USING POSTSUM-PARM                                      
027900     .                                                                    
028000     EJECT                                                                
028100 S11-SKRIV-W33581 SECTION.                                                
028200                                                                          
028300     WRITE UT-POST FROM UT-AREA                                           
028400                                                                          
028500     MOVE 'UTPOST' TO POSTSUM-TRANSTYP                                    
028600     MOVE 'W33581' TO POSTSUM-FDNAMN                                      
028700     MOVE 'W33581D1' TO POSTSUM-DDNAMN2                                   
028800     CALL POSTSUM USING POSTSUM-PARM                                      
028900     .                                                                    
029000     EJECT                                                                
029100 S12-SKRIV-W33582 SECTION.                                                
029200                                                                          
029300     WRITE TOT-POST FROM UT-AREA                                          
029400                                                                          
029500     MOVE 'TOTPOST' TO POSTSUM-TRANSTYP                                   
029600     MOVE 'W33582' TO POSTSUM-FDNAMN                                      
029700     MOVE 'W33581D2' TO POSTSUM-DDNAMN2                                   
029800     CALL POSTSUM USING POSTSUM-PARM                                      
029900     .                                                                    
030000     EJECT                                                                
030100 S99-ABEND SECTION.                                                       
030200                                                                          
030300     SKIP2                                                                
030400     MOVE 'S' TO POSTSUM-OPKOD                                            
030500     CALL POSTSUM USING POSTSUM-PARM                                      
030600     CALL ABEND USING RKOD-ABEND                                          
030700     .                                                                    
030800     EJECT                                                                
030900* --- IMS SEKTIONER ---                                                   
031000                                                                          
031100                                                                          
031200 IMS-GET-BENA   SECTION.                                                  
031300                                                                          
031400     CALL CBLTDLI USING GN BENA-PCB DLI-IO-AREA                           
031500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
031600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900     EJECT                                                                
032000 IMS-GU-WDK601 SECTION.                                                   
032100                                                                          
032200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032300             DELIMITED BY SIZE INTO SSA1                                  
032400     MOVE '  GE' TO GODK-STATUSKODER                                      
032500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
032600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032700     PERFORM IMS-STATUSKONTROLL                                           
032800     .                                                                    
032900     SKIP2                                                                
033000 IMS-STATUSKONTROLL SECTION.                                              
033100                                                                          
033200     SET STATUS-IX TO 1                                                   
033300     SEARCH GODK-STATUS                                                   
033400       AT END                                                             
033500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033600           DELIMITED BY SIZE INTO FELTEXT                                 
033700         DISPLAY FELTEXT                                                  
033800         CALL FELLOG                                                      
033900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034000         CONTINUE                                                         
034100     END-SEARCH                                                           
034200     .                                                                    
