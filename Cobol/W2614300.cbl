000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2614300.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   09/02/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        ARTIKLAR MED TIURPROD                                            
001000*        KOMPLETERAS MED INFO                                             
001100*        FÖR WARNING LAST CALL                                            
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- ARTIKLAR ATT KOMPLETERA                                    
002600     SELECT W26143                     ASSIGN TO W26143D1.                
002700     SKIP2                                                                
002800*          --- FÖRSÄLJNINGSINFO                                           
002900     SELECT WXTR3B                     ASSIGN TO W26143D2.                
003000     SKIP2                                                                
003400*          --- ARTIKLAR FÖR UTSKRIFT                                      
003500     SELECT W26144                     ASSIGN TO W26143D4.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W26143                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W26143      -L.                                                
004600     SKIP3                                                                
004700 FD  WXTR3B                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  -COPY W26164      -L.                                                
005200     SKIP3                                                                
005900 FD  W26144                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W26144 -PRE  UT-  -L.                                     
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006700*    -- CHECKED BY WY2000                                                 
006800 77  IDPGM                       PIC X(8)    VALUE 'W2614300'.            
006900 77  JA                          PIC X       VALUE 'J'.                   
007000 77  NEJ                         PIC X       VALUE 'N'.                   
007100                                                                          
007200 77  W26143-EOF-SW               PIC X       VALUE 'N'.                   
007300     88  END-OF-W26143                       VALUE 'J'.                   
007400                                                                          
007500 77  WXTR3B-EOF-SW               PIC X       VALUE 'N'.                   
007600     88  END-OF-WXTR3B                       VALUE 'J'.                   
007700                                                                          
008000 01  ARBETSAREOR.                                                         
008100     03  W-BEART                 PIC X(25).                               
008200     03  W-SULEVANT-RAAR         PIC S9(9)       COMP-3.                  
008300     03  W-SUTOTBV-RAAR          PIC S9(13)      COMP-3.                  
008400     03  W-SUTOTBV-FRAAR         PIC S9(13)      COMP-3.                  
008500     03  W-SUTOTBV               PIC S9(13)      COMP-3.                  
008600     EJECT                                                                
008700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008800 01  FILLER REDEFINES DAGENS-DATUM.                                       
008900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009200     EJECT                                                                
009300 01  DYNAMISKA-SUBPROGRAM.                                                
009400*                                                                         
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009700     SKIP2                                                                
009800*    --- PARAMETRAR TILL ABEND                                            
009900                                                                          
010000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010300     SKIP2                                                                
010400 01  FELTEXT.                                                             
010500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL POSTSUM                                          
010900*                                                                         
011000*01  -COPY W0005   -PRE  POSTSUM-                                         
011100     EJECT                                                                
011200 01  IN-AREA-START               PIC X(24)   VALUE                        
011300                                 'IN-AREA-START  '.                       
011400     SKIP2                                                                
011500                                                                          
011600*01  AREA -COPY W26143     -PRE IN-                                       
011700     EJECT                                                                
011800 01  REG-AREA-START              PIC X(24)   VALUE                        
011900                                 'REG-AREA-START  '.                      
012000     SKIP2                                                                
012100                                                                          
012200*01  AREA -COPY W26164     -PRE REG-                                      
012300     EJECT                                                                
013000 01  UT-AREA-START               PIC X(24)   VALUE                        
013100                                 'UT-AREA-START  '.                       
013200     SKIP2                                                                
013300                                                                          
013400*01  AREA -COPY W26144     -PRE UT-                                       
013500     EJECT                                                                
013600 PROCEDURE DIVISION.                                                      
013700 MAIN SECTION.                                                            
013800     SKIP2                                                                
013900                                                                          
014000     PERFORM A-INIT                                                       
014200     PERFORM S02-LAES-WXTR3B                                              
014300     PERFORM S01-LAES-W26143                                              
014400     PERFORM UNTIL END-OF-W26143                                          
014500                                                                          
014600       MOVE ZERO              TO  W-SULEVANT-RAAR                         
014700       MOVE ZERO              TO  W-SUTOTBV-RAAR                          
014800       MOVE ZERO              TO  W-SUTOTBV-FRAAR                         
014900       PERFORM UNTIL REG-IDARTNR > IN-IDARTNR                             
015000         IF REG-IDARTNR = IN-IDARTNR                                      
015100            MOVE REG-SULEVANT-RAAR TO  W-SULEVANT-RAAR                    
015200            MOVE REG-SUTOTKBV      TO  W-SUTOTBV-RAAR                     
015300         END-IF                                                           
015400         PERFORM S02-LAES-WXTR3B                                          
015500       END-PERFORM                                                        
015600                                                                          
015700       MOVE SPACE             TO  W-BEART                                 
016800                                                                          
016900       PERFORM C-SKAPA-UTFIL                                              
017000       PERFORM S01-LAES-W26143                                            
017100     END-PERFORM                                                          
017200                                                                          
017300                                                                          
017400     PERFORM Z-FINIT                                                      
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100                                                                          
018200     OPEN INPUT  W26143                                                   
018300                 WXTR3B                                                   
018500                                                                          
018600     OPEN OUTPUT W26144                                                   
018700     SKIP2                                                                
018800     ACCEPT DAGENS-DATUM  FROM DATE                                       
018900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019000     .                                                                    
019100     EJECT                                                                
019200 C-SKAPA-UTFIL SECTION.                                                   
019300                                                                          
019400        COMPUTE W-SUTOTBV =                                               
019500                W-SUTOTBV-RAAR + W-SUTOTBV-FRAAR                          
019600        IF W-SUTOTBV > 15000                                              
019601           OR W-SULEVANT-RAAR > 150                                       
019610           OR (IN-IDFKNGRP = 8535 AND W-SULEVANT-RAAR > 30)               
019700***********OR IN-KVLS NOT > ZERO                                          
019800           CONTINUE                                                       
019900        ELSE                                                              
020000           MOVE IN-IDANSK       TO UT-IDANSK                              
020100           MOVE IN-IDARTNR      TO UT-IDARTNR                             
020200           MOVE IN-IDLEVNR      TO UT-IDLEVNR                             
020300           MOVE IN-IDFKNGRP     TO UT-IDFKNGRP                            
020400           MOVE IN-KDPRODSL     TO UT-KDPRODSL                            
020500           MOVE IN-TIURPROD     TO UT-TIURPROD                            
020600           MOVE IN-FLFKNPRI     TO UT-FLFKNPRI                            
020800           MOVE IN-KVLS         TO UT-KVLS                                
020900           MOVE IN-PRARTSTD     TO UT-PRARTSTD                            
021300           MOVE IN-KDERS        TO UT-KDERS                               
021400           MOVE IN-FLAGGA15     TO UT-FLAGGA15                            
021500           MOVE W-SULEVANT-RAAR TO UT-SULEVANT-RAAR                       
021600           MOVE W-SUTOTBV-RAAR  TO UT-SUTOTBV-RAAR                        
021700           MOVE W-SUTOTBV-FRAAR TO UT-SUTOTBV-FRAAR                       
021800           MOVE IN-TISKPREL     TO UT-TISKPREL                            
021900                                                                          
022000           PERFORM S11-SKRIV-W26144                                       
022100        END-IF                                                            
022200                                                                          
022300     .                                                                    
022400     EJECT                                                                
022500 Z-FINIT SECTION.                                                         
022600     CLOSE W26143                                                         
022700           WXTR3B                                                         
022900           W26144                                                         
023000     SKIP2                                                                
023100     MOVE 'S' TO POSTSUM-OPKOD                                            
023200     CALL POSTSUM USING POSTSUM-PARM                                      
023300     .                                                                    
023400     EJECT                                                                
023500 S01-LAES-W26143  SECTION.                                                
023600     READ W26143 INTO IN-AREA                                             
023700     AT END                                                               
023800        SET END-OF-W26143 TO TRUE                                         
023900                                                                          
024000     NOT AT END                                                           
024100        MOVE 'W26143'   TO POSTSUM-FDNAMN                                 
024200        MOVE 'W26143D1' TO POSTSUM-DDNAMN2                                
024300        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
024400        CALL POSTSUM USING POSTSUM-PARM                                   
024500     END-READ                                                             
024600     .                                                                    
024700     EJECT                                                                
024800 S02-LAES-WXTR3B  SECTION.                                                
024900     READ WXTR3B INTO REG-AREA                                            
025000     AT END                                                               
025100        MOVE 999999999  TO REG-IDARTNR                                    
025200        SET END-OF-WXTR3B TO TRUE                                         
025300                                                                          
025400     NOT AT END                                                           
025500        MOVE 'WXTR3B'   TO POSTSUM-FDNAMN                                 
025600        MOVE 'W26143D2' TO POSTSUM-DDNAMN2                                
025700        MOVE 'WXTR'     TO POSTSUM-TRANSTYP                               
025800        CALL POSTSUM USING POSTSUM-PARM                                   
025900     END-READ                                                             
026000     .                                                                    
026100     EJECT                                                                
027600 S11-SKRIV-W26144 SECTION.                                                
027700                                                                          
027800     WRITE UT-POST FROM UT-AREA                                           
027900                                                                          
028000     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
028100     MOVE 'W26144'   TO POSTSUM-FDNAMN                                    
028200     MOVE 'W26143D4' TO POSTSUM-DDNAMN2                                   
028300     CALL POSTSUM USING POSTSUM-PARM                                      
028400     .                                                                    
028500     EJECT                                                                
028600 S99-ABEND SECTION.                                                       
028700                                                                          
028800     SKIP2                                                                
028900     MOVE 'S' TO POSTSUM-OPKOD                                            
029000     CALL POSTSUM USING POSTSUM-PARM                                      
029100     CALL ABEND USING RKOD-ABEND                                          
029200     .                                                                    
